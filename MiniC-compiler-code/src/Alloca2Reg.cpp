//
// Created by Fan Long on 2020/12/9.
//

/* Alloca to Register Optimization */

//add more header files if your want
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/CFG.h"
#include <set>

using namespace llvm;

// Note in this code, I have used both "iterator" (like in the line 194) methods for going over the basic blocks
// or instuctions or any iterable like maps or sets, to indicate we can loop over them either way.

namespace {
    struct Alloca2RegPass : public FunctionPass {
        static char ID;
        Alloca2RegPass() : FunctionPass(ID) {}

        std::set<AllocaInst*> TargetAllocas; // All good alloca vars that can be promoted
        std::map<BasicBlock*, std::map<AllocaInst*, Value*> > Post; // mapping of basic blocks to their most updated value for every alloca var
        std::map<BasicBlock*, std::map<AllocaInst*, PHINode*> > Pre; // mapping of basic blocks to all created allocas for all alloca vars
        std::set<PHINode*> allPhiNodes; // all PHI nodes created
        std::set<Instruction*> instructionsToDelete; // all store/load/alloca instructions to be deleted 
        std::set<Value*> operands; // all operands used in the program after replacing all of the load instructions

        void clearDataStructures() {
            TargetAllocas.clear();
            Post.clear();
            Pre.clear();
            allPhiNodes.clear();
            instructionsToDelete.clear();
            operands.clear();
        }

        Type* getType(llvm::AllocaInst* alloca) {
            PointerType *ptrType = alloca->getType();
            Type *type = ptrType->getElementType();
        }

        PHINode *createPhiNode(llvm::BasicBlock* basicBlock, llvm::AllocaInst *alloca) {
            Type *type = getType(alloca);
            Instruction *firstInst = basicBlock->getFirstNonPHI();
            PHINode *phi = llvm::PHINode::Create(type, 0, "phi", firstInst);
            for (BasicBlock *predB : llvm::predecessors(basicBlock)) {
                llvm::UndefValue *tmpValue = llvm::UndefValue::get(type);
                phi->addIncoming(tmpValue, predB);
            }
            return phi;
        }

        std::map<AllocaInst*, Value*>* getBasicBlockMappedValueInPost(BasicBlock *bb) {
            auto it = Post.find(bb);
            if (it == Post.end()) {
                return nullptr;
            }
            return &(it->second);
        }

        Value *getAllocaMappedValueInPost(BasicBlock *bb, AllocaInst *alloca) {
            auto map = getBasicBlockMappedValueInPost(bb);
            if (map != nullptr) {
                auto it = map->find(alloca);
                if (it == map->end()) {
                    return nullptr;
                }
                return it->second;
            }
            return nullptr;
        }

        std::map<AllocaInst*, PHINode*>* getBasicBlockMappedValueInPre(BasicBlock *bb) {
            auto it = Pre.find(bb);
            if (it == Pre.end()) {
                return nullptr;
            }
            return &(it->second);
        }

        Value *getAllocaMappedValueInPre(BasicBlock *bb, AllocaInst *alloca) {
            auto map = getBasicBlockMappedValueInPre(bb);
            if (map != nullptr) {
                auto it = map->find(alloca);
                if (it == map->end()) {
                    return nullptr;
                }
                return it->second;
            }
            return nullptr;
        }

        bool isGoodAllocaForPromotion(llvm::AllocaInst* alloca) {
            return TargetAllocas.count(alloca)!= 0;
        }

        void replaceValueOccurances(BasicBlock *basicBlock, llvm::LoadInst *oldValue, Value *newValue) {
            for (Instruction &inst : basicBlock->getInstList()) {
                for (int i = 0; i < inst.getNumOperands(); i++) {
                    if (inst.getOperand(i) == oldValue) {
                        // replace it
                        inst.setOperand(i, newValue);
                    }
                }
            }
        }

        void putPhiInfoToPost(BasicBlock* basicBlock, AllocaInst* alloca, Value* value) {
            auto bbMap = getBasicBlockMappedValueInPost(basicBlock);
            if (bbMap == nullptr) {
                // This basicBlock is not in the Pre yet
                std::map<AllocaInst *, Value *> bbMap;
                bbMap.insert(std::make_pair(alloca, value));
                Post.insert(std::make_pair(basicBlock, bbMap));
            } else {
                bbMap->insert(std::make_pair(alloca, value));
            }
        }

        void putPhiInfoToPre(BasicBlock *basicBlock, AllocaInst *alloca, PHINode *phiNode) {
            auto bbMap = getBasicBlockMappedValueInPre(basicBlock);
            if (bbMap == nullptr) {
                // This basicBlock is not in the pre yet
                std::map<AllocaInst *, PHINode *> phiMap;
                phiMap.insert(std::make_pair(alloca, phiNode));
                Pre.insert(std::make_pair(basicBlock, phiMap));
            } else {
                bbMap->insert(std::make_pair(alloca, phiNode));
            }
        }

        void replaceSingleEdgePhi(PHINode *phi) {
            Value *value = phi->getIncomingValue(0);
            phi->replaceAllUsesWith(value);
        }

        bool optimizeAllPhiNodes(std::set<PHINode *> &allPhiNodes, std::set<PHINode *> &unusedPhiNodes) {
            bool atLeastOnePhiDeleted = false;
            for (std::set<PHINode *>::iterator it = allPhiNodes.begin(); it != allPhiNodes.end();) {
                PHINode *phi = *it;
                if (unusedPhiNodes.count(phi) == 0) {  // these are used PHI nodes
                    bool deleted = false;
                    deleted = optimizePhiNode(phi);
                    if (deleted) {
                        it = allPhiNodes.erase(it);
                        phi->eraseFromParent();
                        atLeastOnePhiDeleted = true;
                        break;
                    }
                    if (!deleted) {
                        it++;
                    }
                } else {
                    it++;
                }
            }

            return atLeastOnePhiDeleted;
        }

        bool optimizePhiNode(PHINode *phi) {
            // Check if the phi node is a single edged node
            if (phi->getNumIncomingValues() <= 1) {
                replaceSingleEdgePhi(phi);
                return true;
            } else {
                
                // Check if the phi node has a value equal it itself, i.e. a self referencing phi
                for (int i = 0; i < phi->getNumIncomingValues(); i++) {
                    Value *value = phi->getIncomingValue(i);
                    if (value == phi || llvm::isa<llvm::UndefValue>(value)) {  // note every phi instruction is a subclass of Value, everything in LLVM is a Value
                        if (phi->getNumIncomingValues() <= 2) {
                            phi->removeIncomingValue(i, true);
                            replaceSingleEdgePhi(phi);
                            return true;
                        }
                    }
                }

                // Check if the phi node has all equal or UndefVlaue edges
                if (phi->hasConstantOrUndefValue()) {
                    if (llvm::isa<llvm::UndefValue>(phi->getIncomingValue(0))) {
                        phi->eraseFromParent();
                        return true;
                    } else {
                        // since a phi node with all equal edges is like a single edged phi. i.e. we need the value of one its edges only
                        replaceSingleEdgePhi(phi);
                        return true;
                    }
                }
            }
            return false;
        }

        bool needsOptimization(PHINode *phi) {
            if (phi->getNumIncomingValues() <= 1) {
                return true;
            }
            for (int i = 0; i < phi->getNumIncomingValues() - 1; i++) {
                Value *preValue = phi->getIncomingValue(i);
                for (int t = i + 1; t < phi->getNumIncomingValues(); t++) {
                    Value *value = phi->getIncomingValue(t);
                    if (value == preValue || preValue == phi || value == phi
                        || llvm::isa<llvm::UndefValue>(value) || llvm::isa<llvm::UndefValue>(value)) {
                        return true;
                    }
                }
            }
            return false;
        }

        void collectTargetAllocas(Function &F) {
            for (Function::iterator bbIt = F.begin(); bbIt != F.end(); bbIt++) {
                BasicBlock *basicBlock = &(*bbIt);
                for (BasicBlock::iterator instIt = basicBlock->begin(); instIt != basicBlock->end(); instIt++) {
                    Instruction *instruction = &(*instIt);
                    if (llvm::isa<llvm::AllocaInst>(instruction)) {
                        Type *type = getType((AllocaInst *)instruction);
                        // this alloca is a good candidate for register promotion since it is not a pointer
                        // type and its not an aggregate type
                        if (type->isIntegerTy()) {  
                            TargetAllocas.insert((AllocaInst *)instruction);
                            instructionsToDelete.insert(instruction);
                        }
                        // Note that in the normal languages where we have a pointer type, we
                        // should further check if no one has taken an address of this int typed alloca, also.
                        // if yes, we cannot promote it.
                        // But in MiniC, we do not have these concepts, so we are good with these amounts of checks.
                        // In MiniC, we only check to remove the alloca vars of tyoe int, since we have only bool and int types.
                        // Also we do not touch the array types (i.e. aggregate types).
                    }
                }
           }
        }

        void collectAllInstructionsOperands(Function &F) {
            for (BasicBlock& basicBlock: F) {
                for (Instruction &inst : basicBlock.getInstList()) {
                    for (int i = 0; i < inst.getNumOperands(); i++) {
                        operands.insert(inst.getOperand(i));
                    }
                }
            }
        }
        // 1. Create PHI nodes for all alloca vars in all basic blocks except the "entry".
        // Update the maps: Replace "load" instructions with a PHI or a value and
        // update values of alloca vars with "store" instructions.
        virtual bool runOnFunction(Function &F) {
            errs() << "Working on function called " << F.getName() << "!\n";
            collectTargetAllocas(F);
            for (Function::iterator bbIt = F.begin(); bbIt != F.end(); bbIt++) {
                BasicBlock *basicBlock = &(*bbIt);
                // add the phi nodes for all alloca vars to this bb
                if (basicBlock != &(F.getEntryBlock())) {
                    for (auto &var : TargetAllocas) {
                        llvm::PHINode *phi = createPhiNode(basicBlock, (llvm::AllocaInst *)var);
                        putPhiInfoToPre(basicBlock, var, phi);
                        putPhiInfoToPost(basicBlock, var, (llvm::Value *)phi);
                        allPhiNodes.insert(phi);
                    }
                }
                for (BasicBlock::iterator instIt = basicBlock->begin(); instIt != basicBlock->end(); instIt++) {
                    Instruction *instruction = &(*instIt);
                    // now check the load/store instructions
                    if (llvm::isa<llvm::LoadInst>(instruction)) {
                        llvm::Value *allocaVar = ((LoadInst*)instruction)->getPointerOperand();  // allocaVar is in fact an AllocaInst instruction
                        if (isGoodAllocaForPromotion((llvm::AllocaInst*)allocaVar)) {
                            // at this point, we know Post is filled with mappings of alloca vars
                            // to either a PHI node or a concrete value from a "store"
                            Value* value = getAllocaMappedValueInPost(basicBlock, (llvm::AllocaInst *)allocaVar);
                            // replace the occurances of this allocated var's load instuction with
                            // the current saved value every where in the code
                            replaceValueOccurances(basicBlock, (LoadInst *)instruction, value);
                            instructionsToDelete.insert(instruction);
                        }
                    } else if (llvm::isa<llvm::StoreInst>(instruction)) {
                        llvm::Value *allocaVar = ((StoreInst*)instruction)->getPointerOperand();  // allocaVar is in fact an AllocaInst instruction
                        if (isGoodAllocaForPromotion((llvm::AllocaInst*)allocaVar)) {
                            llvm::Value *valueOperand = ((StoreInst *)instruction)->getValueOperand();
                            auto map = getBasicBlockMappedValueInPost(basicBlock);
                            if (!map) {  // no entry for this basicBlock exists yet. This will happen for "entry" block.
                                std::map<AllocaInst *, Value *> bbMap;
                                bbMap.insert(std::make_pair((AllocaInst *)allocaVar, valueOperand));
                                Post.insert(std::make_pair(basicBlock, bbMap));
                            } else {
                                // update the allocaVar's value
                                (*map)[(AllocaInst*)allocaVar] = valueOperand;
                            }
                            instructionsToDelete.insert(instruction);
                        }
                    }
                }
            }

            // 2. Fill in all incoming edges of PHI nodes in Pre, by reading info from the Post
            for (auto &preElement: Pre) {
                std::map<AllocaInst *, PHINode *> &valueMap = preElement.second;
                for (auto &m: valueMap) {
                    AllocaInst *alloca = m.first;
                    PHINode *phi = m.second;
                    for (int i = 0; i < phi->getNumIncomingValues(); i++) {
                        BasicBlock *bb = phi->getIncomingBlock(i);
                        Value *value = getAllocaMappedValueInPost(bb, alloca);
                        // entry block might not have some mappings, since we did not add any PHIs for it
                        // but that is ok, since if a PHI node wants to get info from a non-exsiting PHI
                        // node of the entry, that means that PHI node will not be used, and so 
                        // it will deleted later on anyways
                        if (value) { 
                            phi->setIncomingValue(i, value);
                        }
                    }
                }
            }

            // 3. Collect all "used" instructions in the program at this point so that we can remove the 
            // unused PHI nodes later
            collectAllInstructionsOperands(F);

            // 4. Find all "unused" PHI nodes 
            std::set<PHINode *> unusedPhiNodes;
            for (std::set<PHINode *>::iterator it = allPhiNodes.begin(); it != allPhiNodes.end(); it++) {
                PHINode *phi = *it;
                if (operands.count(phi) == 0) {
                    unusedPhiNodes.insert(phi);                    
                }
            }

            // 5. Remove all promotable load, store, and allca instructions
            std::for_each(instructionsToDelete.begin(), instructionsToDelete.end(), [](Instruction* inst) {inst->eraseFromParent();});

            // 6. Remove all phi nodes which have one incoming edge, are self referencing,
            // or have constant value and use their value instead
            bool optimizationNeeded = optimizeAllPhiNodes(allPhiNodes, unusedPhiNodes);
            while (optimizationNeeded) {
                optimizationNeeded = optimizeAllPhiNodes(allPhiNodes, unusedPhiNodes);
            }

            // 7. Remove all unused PHI nodes
            for (auto &phi: unusedPhiNodes) {
                phi->eraseFromParent();
            }

            // 8. Clear the data structures used in this function pass. Note this is needed since the data
            // structures of this struct is global among all of the functions of the module on which this
            // pass is being run.
            clearDataStructures();

            return true;
        }
    };
}

char Alloca2RegPass::ID = 0;

static RegisterPass<Alloca2RegPass> X("alloca2reg", "Alloca-to-register pass for minic", false, false);

// Automatically enable the pass.
// http://adriansampson.net/blog/clangpass.html
static void registerAlloca2RegPass(const PassManagerBuilder &,
                                    legacy::PassManagerBase &PM) {
    PM.add(new Alloca2RegPass());
}
static RegisterStandardPasses
        RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible,
                       registerAlloca2RegPass);
