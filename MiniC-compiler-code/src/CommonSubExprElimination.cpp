/* Common Subexpression Elimination Optimization */

#include <set>

#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

namespace {
    struct CommonSubExprEliminationPass : public FunctionPass {
        static char ID;
        CommonSubExprEliminationPass() : FunctionPass(ID) {}

        std::set<Instruction*> instructionsToDelete;
        
        virtual bool runOnFunction(Function &F) {
            errs() << "Running CommonSubExprElimination on function called " << F.getName() << "!\n";
            bool needOptimization = true;
            while (needOptimization) {
                for (Function::iterator basicBlockIt = F.begin(); basicBlockIt != F.end(); basicBlockIt++) {
                    BasicBlock *basicBlock = &(*basicBlockIt);
                    for (BasicBlock::iterator it1 = basicBlock->begin(); it1 != basicBlock->end(); it1++) {
                        Instruction *instruction1 = &(*it1);
                        llvm::Value *loadPtrOperand = nullptr;
                        if (llvm::isa<llvm::LoadInst>(instruction1)) {
                            loadPtrOperand = ((LoadInst *)instruction1)->getPointerOperand();
                        }
                        for (BasicBlock::iterator it2 = basicBlock->begin(); it2 != basicBlock->end(); it2++) {
                            Instruction *instruction2 = &(*it2);
                            if (llvm::isa<llvm::StoreInst>(instruction2) &&
                                ((LoadInst *)instruction2)->getPointerOperand() == loadPtrOperand) {
                                break;
                            }
                            if (instruction1 != instruction2 &&
                                instructionsToDelete.count(instruction2) == 0 &&
                                instructionsToDelete.count(instruction1) == 0 &&
                                instruction1->isIdenticalTo(instruction2)) {
                                errs() << "Identical instructions ..........\n";
                                instruction1->print(errs());
                                errs() << "\n";
                                instruction2->print(errs());
                                errs() << "\n ..................... \n";
                                instructionsToDelete.insert(instruction2);
                                instruction2->replaceAllUsesWith(instruction1);  // replace the uses of instruction2 with instruction1
                            }
                        }
                    }
                }
                // Unless we do not have anything to delete, optimzation passes (i.e. this "while loop")
                // should still run. Since deleting an instruction create new common sub expr elimination opportunites.
                if (!instructionsToDelete.size()) { 
                    needOptimization = false;
                }
                std::for_each(instructionsToDelete.begin(), instructionsToDelete.end(), [](Instruction *inst) {
                    inst->eraseFromParent();
                });
                instructionsToDelete.clear();
            }

            // 2. Clear the data structures used in this function pass. Note this is needed since the data
            // structures of this struct is global among all of the functions of the module on which this
            // pass is being run.
            instructionsToDelete.clear();

            return true;
        }
    };
}  // namespace

char CommonSubExprEliminationPass::ID = 1;

static RegisterPass<CommonSubExprEliminationPass> X("commonSubExprElimination", "Common-SubExpr-Elimination pass for minic", false, false);

// Automatically enable the pass.
// http://adriansampson.net/blog/clangpass.html
static void registerCommonSubExprEliminationPass(const PassManagerBuilder &, legacy::PassManagerBase &PM) {
     PM.add(new CommonSubExprEliminationPass()); 
}
static RegisterStandardPasses RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible, registerCommonSubExprEliminationPass);
