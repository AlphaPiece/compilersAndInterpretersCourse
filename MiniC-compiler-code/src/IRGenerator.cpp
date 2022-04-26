//
// Created by Fan Long on 2020/12/6.
//

//add more header files if your want
#include "IRGenerator.h"
#include "llvm/IR/Module.h"
#include "Declarations.h"
#include "Program.h"
#include "Exprs.h"
#include "Statements.h"
#include <iostream>

namespace minicc {

    /********************** member helper functions **********************/

    llvm::Value* IRGenerator::createGEPInstruction(VarReference *varRef, llvm::Value *arrPtrValue) {
        Expr* indexExpr = varRef->indexExpr();
        llvm::Value *indexExprLlvmValue = getLlvmValueForExpr(indexExpr);
        llvm::Constant *zeroLlvmValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0, true);
        std::vector<llvm::Value*> indexVector;
        indexVector.push_back(zeroLlvmValue);
        indexVector.push_back(indexExprLlvmValue);
        return TheBuilder->CreateGEP(arrPtrValue, indexVector);
    }

    llvm::Value* IRGenerator::translateLogicalBinaryExpr(BinaryExpr *expr) {

        llvm::Function *parentFunc = TheBuilder->GetInsertBlock()->getParent();
        llvm::BasicBlock *currentBasicBlock = TheBuilder->GetInsertBlock();
        llvm::BasicBlock *slowBasicBlock = llvm::BasicBlock::Create(*TheContext, "logical-slow-bb", parentFunc);
        llvm::BasicBlock *outBasicBlock = llvm::BasicBlock::Create(*TheContext, "logical-out-bb", parentFunc);

        auto firstExpr = expr->getChild(0);
        firstExpr->accept(this);
        llvm::Value *firstExprValue = getLlvmValueForExpr((Expr *)firstExpr);
        llvm::Constant *trueLlvmValue = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 1, true);
        llvm::Value *icmpLlvmValue = TheBuilder->CreateICmpEQ(firstExprValue, trueLlvmValue);

        minicc:Expr::ExprOpcode opCode = expr->opcode();
        if (opCode == Expr::And) {
            TheBuilder->CreateCondBr(icmpLlvmValue, slowBasicBlock, outBasicBlock);
        } else if (opCode == Expr::Or) {
            TheBuilder->CreateCondBr(icmpLlvmValue, outBasicBlock, slowBasicBlock);
        }

        TheBuilder->SetInsertPoint(slowBasicBlock);
        auto secondExpr = expr->getChild(1);
        secondExpr->accept(this);
        TheBuilder->CreateBr(outBasicBlock);
        TheBuilder->SetInsertPoint(outBasicBlock); // jump to the outBasicBlock

        llvm::Value* secondTermValue = getLlvmValueForExpr((Expr*)secondExpr);

        llvm::Type *boolLlvmType = getLlvmValueOfType(minicc::Type(minicc::Type::Bool));
        llvm::PHINode* phiLlvmValue = (llvm::PHINode*)TheBuilder->CreatePHI(boolLlvmType, 2);
        if (opCode == Expr::And) {
            phiLlvmValue->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 0, true), currentBasicBlock);
            phiLlvmValue->addIncoming(secondTermValue, slowBasicBlock);
        } else if (opCode == Expr::Or) {
            phiLlvmValue->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 1, true), currentBasicBlock);
            phiLlvmValue->addIncoming(secondTermValue, slowBasicBlock);
        }
        return (llvm::Value*)phiLlvmValue;
    }

    void IRGenerator::setParametersLlvmValues(FuncDeclaration *func, VarSymbolTable *varTable) {
        for (int i = 0; i < func->numParameters(); i++) {
            Parameter *param = func->parameter(i);
            std::string paramName = param->name();
            VarSymbolEntry *varEntry = varTable->findSymbol(paramName);
            Type t = varEntry->VarType;
            llvm::Type *varType = getLlvmValueOfType (t);
            llvm::Value *llvmValue = nullptr;

            if (t.arrayBound() != 0) {  // it is an array
                size_t arrBound = t.arrayBound();
                llvm::Value *arrBoundLLVMValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), arrBound, true);
                llvm::ArrayType *llvmArrayType = llvm::ArrayType::get(varType, arrBound);

                llvm::Constant *intValue = llvm::ConstantInt::get(varType, arrBound, true);
                llvmValue = TheBuilder->CreateAlloca(llvmArrayType, arrBoundLLVMValue, paramName);
            } else {
                llvmValue = TheBuilder->CreateAlloca(varType, nullptr, paramName);
            }
            varTable->setLLVMValue(paramName, llvmValue);
        }
    }

    void IRGenerator::allocateParameters(FuncDeclaration *func, llvm::Function *llvmFunc) {
        
        ScopeStatement *funcBody = func->body();
        VarSymbolTable *varTable = funcBody->scopeVarTable();

        for (int i = 0; i < func->numParameters(); i++) {
            Parameter *param = func->parameter(i);
            std::string paramName = param->name();
            VarSymbolEntry *symbolEntry = varTable->findSymbol(paramName);
            minicc::Type t = symbolEntry->VarType;
            llvm::Type *varType = getLlvmValueOfType(t);
            llvm::Value *llvmPtrValue = nullptr;

            if (t.arrayBound() != 0) {  // it is an array
                size_t arrBound = t.arrayBound();
                llvm::Value *arrBoundLLVMValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), arrBound, true);
                llvm::ArrayType *llvmArrayType = llvm::ArrayType::get(varType, arrBound);
                llvmPtrValue = TheBuilder->CreateAlloca(llvmArrayType, arrBoundLLVMValue, paramName);
            } else {
                llvmPtrValue = TheBuilder->CreateAlloca(varType, nullptr, paramName);
            }

            // create a store to parameter registers which will hold the
            // value of args when the function is called
            llvm::Argument* p = llvmFunc->getArg(i);
            llvm::Value *argLlvmValue = TheBuilder->CreateStore((llvm::Value *)p, llvmPtrValue);

            // llvmPtrValue is the local (i.e. stack) ptr to the paramName
            varTable->setLLVMValue(paramName, llvmPtrValue);
        }
    }

    void IRGenerator::allocateLocalVars(VarSymbolTable *varTable, std::vector<std::string> varNamesVector) {
        
        for (int i = 0; i < varNamesVector.size(); i++) {
            std::string varName = varNamesVector[i];
            VarSymbolEntry *symbolEntry = varTable->findSymbol(varName);
            minicc::Type t = symbolEntry->VarType;
            llvm::Type *varType = getLlvmValueOfType(t);
            llvm::Value *llvmPtrValue = nullptr;

            if (t.arrayBound() != 0) {
                size_t arrBound = t.arrayBound();
                llvm::Value *arrBoundLLVMValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), arrBound, true);
                llvm::ArrayType *llvmArrayType = llvm::ArrayType::get(varType, arrBound);
                llvmPtrValue = TheBuilder->CreateAlloca(llvmArrayType, arrBoundLLVMValue, varName);
            } else {
                llvmPtrValue = TheBuilder->CreateAlloca(varType, nullptr, varName);
            }
            // llvmPtrValue is the local (stack) ptr to the varName
            varTable->setLLVMValue(varName, llvmPtrValue);
        }
    }

    void IRGenerator::allocateGlobalVars(VarSymbolTable *varTable, std::vector<std::string> varNamesVector) {

        for (int i = 0; i < varNamesVector.size(); i++) {
            std::string varName = varNamesVector[i];
            VarSymbolEntry *symbolEntry = varTable->findSymbol(varName);
            Type t = symbolEntry->VarType;
            llvm::Type *varType = getLlvmValueOfType(t);
            llvm::GlobalVariable *llvmPtrValue = nullptr;
            if (t.arrayBound() != 0) {
                size_t arrBound = t.arrayBound();
                llvm::ArrayType *llvmArrayType = llvm::ArrayType::get(varType, arrBound);
                llvm::ConstantAggregateZero* arrayInitWithZeros = llvm::ConstantAggregateZero::get(llvmArrayType);
                llvmPtrValue = new llvm::GlobalVariable(*TheModule, llvmArrayType, false, llvm::GlobalVariable::CommonLinkage, arrayInitWithZeros, varName);
            } else {
                llvmPtrValue = new llvm::GlobalVariable(*TheModule, varType, false, llvm::GlobalVariable::CommonLinkage, llvm::ConstantInt::get(varType, 0, true), varName);
            }
            // llvmPtrValue is the global ptr to the varName
            varTable->setLLVMValue(varName, llvmPtrValue);
        }
    }

    /*****************************************************************************/

    void IRGenerator::visitProgram(Program *prog) {
        //Initlize llvm module and builder
        TheModule = std::make_unique<llvm::Module>(ModuleName, *TheContext);
        TheBuilder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

        // insert all the functions into the module
        FuncSymbolTable *funcTable = prog->funcTable();
        for (auto it = funcTable->begin(); it != funcTable->end(); it++) {
            std::string funcName = it->first;
            FuncSymbolEntry funcEntry = it->second;
            minicc::Type returnType = funcEntry.ReturnType;
            std::vector<minicc::Type> parameterVector = funcEntry.ParameterTypes;
            std::vector<llvm::Type *> parametersLlvmValuesVector;
            for (int i = 0; i < parameterVector.size(); i++) {
                parametersLlvmValuesVector.push_back(getLlvmValueOfType(parameterVector[i]));
            }

            llvm::FunctionType *llvmFuncType = llvm::FunctionType::get(getLlvmValueOfType(returnType), parametersLlvmValuesVector, false);
            llvm::Function *llvmFunc = llvm::Function::Create(llvmFuncType, llvm::Function::ExternalLinkage, funcName, TheModule.get());
        }

        // go over your children nodes
        ASTVisitor::visitProgram(prog);
    }

    void IRGenerator::visitVarDecl(VarDeclaration *decl) {

        ASTNode* scope = decl->getParentScope();
        VarSymbolTable *varTable = scope->scopeVarTable();
        std::vector<std::string> varNamesVector;
        for (int i = 0; i < decl->numVarReferences(); i++) {
            VarReference *varRef = decl->varReference(i);
            std::string varName = varRef->identifier()->name();
            varNamesVector.push_back(varName);
        }
        // Var symbol tables holds the pointers of global or local vars
        // while the helper map "ExprValueMap" is used to hold the llvm
        // registers during our AST traversal
        if (scope->isProgram()) {
            // global variable
            allocateGlobalVars(varTable, varNamesVector);
        } else {
            // local variable
            allocateLocalVars(varTable, varNamesVector);
        }
    }

    void IRGenerator::visitFuncDecl(FuncDeclaration *func) {

        std::string funcName = func->name();
        llvm::Function *llvmFunc = TheModule->getFunction(funcName);
        if (func->hasBody()) {
            ScopeStatement *funcBody = func->body();
            VarSymbolTable *varTable = funcBody->scopeVarTable();
            // create a basic block
            llvm::BasicBlock *bodyBasicBlock = llvm::BasicBlock::Create(*TheContext, "entry", llvmFunc);
            // enter the bodyBasicBlock, so that any instruction from now on can be put
            // inside this block
            TheBuilder->SetInsertPoint(bodyBasicBlock);
            allocateParameters(func, llvmFunc);
            // go over your childen nodes
            ASTVisitor::visitScope(funcBody);

            // create an explicit exit instruction in case the function's return is void
            // but the last statement is not a return
            // This is becuase every block should 
            Statement* lastStatement = nullptr;

            // if the current block has not been terminated, that means the function has
            // return type "void", so terminate it with a void return
            if (!TheBuilder->GetInsertBlock()->getTerminator()) {
                TheBuilder->CreateRetVoid();
            }
        }
    }

    void IRGenerator::visitIfStmt(IfStatement *stmt) {

        llvm::Function *parentFunc = TheBuilder->GetInsertBlock()->getParent();
        auto condExpr = stmt->getChild(0);
        condExpr->accept(this);
        llvm::Value *condExprLlvmValue = getLlvmValueForExpr((Expr*)condExpr);

        // condExprLlvmValue has a bool value and we can directly use it in CreateCondBr
        llvm::BasicBlock *thenBasicBlock = llvm::BasicBlock::Create(*TheContext, "then-bb", parentFunc);
        llvm::BasicBlock *elseBasicBlock = llvm::BasicBlock::Create(*TheContext, "else-bb"); // add this block only if the else stmt exists
        llvm::BasicBlock *outBasicBlock = llvm::BasicBlock::Create(*TheContext, "out-bb", parentFunc);
        if (stmt->hasElse()) {
            TheBuilder->CreateCondBr(condExprLlvmValue, thenBasicBlock, elseBasicBlock);
        } else {
            TheBuilder->CreateCondBr(condExprLlvmValue, thenBasicBlock, outBasicBlock);
        }

        // thenStmt
        TheBuilder->SetInsertPoint(thenBasicBlock);
        auto thenStmt = stmt->getChild(1);
        thenStmt->accept(this);
        if (!TheBuilder->GetInsertBlock()->getTerminator()) {
            TheBuilder->CreateBr(outBasicBlock);
        }

        // elseStmt
        if (stmt->hasElse()) {
            // now add the else block
            parentFunc->getBasicBlockList().push_back(elseBasicBlock);
            TheBuilder->SetInsertPoint(elseBasicBlock);
            // go over the childern nodes
            auto elseStmt = stmt->getChild(2);
            elseStmt->accept(this);
            if (!TheBuilder->GetInsertBlock()->getTerminator()) {
                TheBuilder->CreateBr(outBasicBlock);
            }
        }

        // go to the outBasicBlock so that anything outside the for-loop be translated inside that
        TheBuilder->SetInsertPoint(outBasicBlock);
    }

    void IRGenerator::visitForStmt(ForStatement *stmt) {

        auto initExpr = stmt->getChild(0);
        if (initExpr != nullptr) {
            initExpr->accept(this);
        }

        llvm::Function *parentFunc = TheBuilder->GetInsertBlock()->getParent();
        llvm::BasicBlock *condBasicBlock = llvm::BasicBlock::Create(*TheContext, "cond-bb", parentFunc);
        llvm::BasicBlock *bodyBasicBlock = llvm::BasicBlock::Create(*TheContext, "body-bb", parentFunc);
        llvm::BasicBlock *outBasicBlock = llvm::BasicBlock::Create(*TheContext, "out-bb", parentFunc);

        TheBuilder->CreateBr(condBasicBlock);
        TheBuilder->SetInsertPoint(condBasicBlock);
        auto condExpr = stmt->getChild(1);
        condExpr->accept(this);
        llvm::Value *condExprLlvmValue = getLlvmValueForExpr((Expr*)condExpr);

        // condExprLlvmValue has a bool value and we can directly use it in CreateCondBr
        // set the conditional branching to body or out depending on the condExpr
        llvm::Value *condBranchLlvmValue = TheBuilder->CreateCondBr(condExprLlvmValue, bodyBasicBlock, outBasicBlock);

        TheBuilder->SetInsertPoint(bodyBasicBlock);

        // save the outBasicBlock such that in case we have a "break", we can jump out into it
        loopsOutBasicBlockStack.push_back(outBasicBlock);

        auto bodyStmt = stmt->getChild(3);
        bodyStmt->accept(this); 

        // Only if the current branch has not been already terminated, add the iter translation
        llvm::BasicBlock *currentBlock = TheBuilder->GetInsertBlock();
        if (!currentBlock->getTerminator()) {
            auto iterExpr = stmt->getChild(2);
            if (iterExpr != nullptr){
                iterExpr->accept(this);
            }
            TheBuilder->CreateBr(condBasicBlock);
        }
       
        // go to the outBasicBlock so that anything outside the for-loop be translated inside that
        TheBuilder->SetInsertPoint(outBasicBlock);
        // delete the stack of "out" basic blocks related to this "for" loop, since we are leaving it
        loopsOutBasicBlockStack.pop_back();
    }

    void IRGenerator::visitWhileStmt(WhileStatement *stmt) {

        llvm::Function *parentFunc = TheBuilder->GetInsertBlock()->getParent();
        llvm::BasicBlock *condBasicBlock = llvm::BasicBlock::Create(*TheContext, "cond-bb", parentFunc);
        llvm::BasicBlock *bodyBasicBlock = llvm::BasicBlock::Create(*TheContext, "body-bb", parentFunc);
        llvm::BasicBlock *outBasicBlock = llvm::BasicBlock::Create(*TheContext, "out-bb", parentFunc);

        // check the cond stmt
        TheBuilder->SetInsertPoint(condBasicBlock);
        auto condExpr = stmt->getChild(0);
        condExpr->accept(this);
        llvm::Value *condExprLlvmValue = getLlvmValueForExpr((Expr*)condExpr);
        // condExprLlvmValue has a bool value and we can directly use it in CreateCondBr
        // set the conditional branching to body or out depending on the condExpr
        llvm::Value *condBranchLlvmValue = TheBuilder->CreateCondBr(condExprLlvmValue, bodyBasicBlock, outBasicBlock);

        // check the body
        TheBuilder->SetInsertPoint(bodyBasicBlock);
        auto bodyStmt = stmt->getChild(1);
        bodyStmt->accept(this);

        // save the out bb
        loopsOutBasicBlockStack.push_back(outBasicBlock);

        // set the current branch (which might not be the body-bb anymore) 
        // to branch to condBasicBlock so that it go back and check the while loop's condition
        llvm::BasicBlock *currentBlock = TheBuilder->GetInsertBlock();
        if (!currentBlock->getTerminator()) {
            TheBuilder->CreateBr(condBasicBlock);
        }

        // go to the outBasicBlock so that anything outside the for-loop be translated inside that
        TheBuilder->SetInsertPoint(outBasicBlock);
        // delete the stack of "out" basic blocks related to this "for" loop
        loopsOutBasicBlockStack.pop_back();
    }

    void IRGenerator::visitReturnStmt(ReturnStatement *stmt) {

        ASTVisitor::visitReturnStmt(stmt);

        if(stmt->hasReturnExpr()) {
            Expr* returnExpr = stmt->returnExpr();
            TheBuilder->CreateRet(getLlvmValueForExpr(returnExpr));
        } else {
            TheBuilder->CreateRetVoid();
        }
    }

    void IRGenerator::visitBreakStmt(BreakStatement *stmt) {
        // jump out to the most recent loop's outBasicBlock.
        // Note here the top of stack is the back of the vector of outBasicBlock, 
        // so we jump into that 
        llvm::BasicBlock *outBasicBlock = loopsOutBasicBlockStack.back();
        TheBuilder->CreateBr(outBasicBlock);
    }

    void IRGenerator::visitUnaryExpr(UnaryExpr *expr) {

        ASTVisitor::visitUnaryExpr(expr);
        llvm::Value *unaryExprLlvmValue = getLlvmValueForExpr((Expr*)expr->getChild(0));
        llvm::Value *exprLlvmValue = nullptr;
        Expr::ExprOpcode opCode = expr->opcode();
        if (opCode == Expr::Not) {
            exprLlvmValue = TheBuilder->CreateNot(unaryExprLlvmValue);
        } else if (opCode == Expr::Sub) {
            exprLlvmValue = TheBuilder->CreateNeg(unaryExprLlvmValue);
        }
        setLlvmValueForExpr(expr, exprLlvmValue);
    }

    void IRGenerator::visitBinaryExpr(BinaryExpr *expr) {

        llvm::Value *exprLlvmValue = nullptr;
        Expr::ExprOpcode opCode = expr->opcode();

        // Do lazy logical evaluation for && and ||
        if (opCode == Expr::And || opCode == Expr::Or) {
            exprLlvmValue = translateLogicalBinaryExpr(expr);
        } else {
            auto firstExpr = expr->getChild(0);
            firstExpr->accept(this);
            auto secondExpr = expr->getChild(1);
            secondExpr->accept(this);
            llvm::Value *firstExprLlvmValue = getLlvmValueForExpr((Expr *)firstExpr);
            llvm::Value *secondExprLlvmValue = getLlvmValueForExpr((Expr *)secondExpr);

            switch (opCode) { 
                case Expr::Add:
                    exprLlvmValue = TheBuilder->CreateAdd(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::Sub:
                    exprLlvmValue = TheBuilder->CreateSub(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::Mul:
                    exprLlvmValue = TheBuilder->CreateMul(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::Div:
                    exprLlvmValue = TheBuilder->CreateSDiv(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::Less:
                    exprLlvmValue = TheBuilder->CreateICmpSLT(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::LessEqual:
                    exprLlvmValue = TheBuilder->CreateICmpSLE(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::Greater:
                    exprLlvmValue = TheBuilder->CreateICmpSGT(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::GreaterEqual:
                    exprLlvmValue = TheBuilder->CreateICmpSGE(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::Equal:
                    exprLlvmValue = TheBuilder->CreateICmpEQ(firstExprLlvmValue, secondExprLlvmValue);
                    break;  
                case Expr::NotEqual:
                    exprLlvmValue = TheBuilder->CreateICmpNE(firstExprLlvmValue, secondExprLlvmValue);
                    break; 
            }
        }

       setLlvmValueForExpr(expr, exprLlvmValue);
    }

    void IRGenerator::visitCallExpr(CallExpr *expr) {

        std::string funcName = expr->callee()->name();
        llvm::Function *func = TheModule->getFunction(funcName);

        ASTVisitor::visitCallExpr(expr);

        std::vector<llvm::Value*> argsVector;
        for(int i=0; i < expr->numArgs(); i++) {
            Expr* arg = expr->arg(i);
            llvm::Value *argLlvmValue = getLlvmValueForExpr(arg);
            argsVector.push_back(argLlvmValue);
        }

        llvm::Value* funcCallResult = TheBuilder->CreateCall(func, argsVector);
        setLlvmValueForExpr(expr, funcCallResult);
    }

    void IRGenerator::visitVarExpr(VarExpr *expr) {

        ASTVisitor::visitVarExpr(expr); // this line can be deleted also since we never need to visit the childern of a varExpr in code genration, but I just kept it.
        VarReference *varRef = (VarReference *)expr->getChild(0);
        std::string varName = varRef->identifier()->name();
        VarSymbolTable *varTable = expr->locateDeclaringTableForVar(varName);
        VarSymbolEntry *varEntry = varTable->findSymbol(varName);
        // varEntry.LLVMValue is the base ptr, but if the the varRef is
        // an array we should find the correct offset of it by GEP instruction
        llvm::Value *prtToLoadFrom = varEntry->LLVMValue;
        if (varRef->isArray()) {
            prtToLoadFrom = createGEPInstruction(varRef, varEntry->LLVMValue);
        }

        llvm::Value* loadedValue = TheBuilder->CreateLoad(prtToLoadFrom);
        setLlvmValueForExpr(expr, loadedValue);
    }

    void IRGenerator::visitAssignmentExpr(AssignmentExpr *expr) {

        ASTVisitor::visitAssignmentExpr(expr);
        VarReference *leftSideVarRef = (VarReference*)expr->getChild(0);
        std::string varName = leftSideVarRef->identifier()->name();
        // varName could be a local or global var
        VarSymbolTable *varTable = expr->locateDeclaringTableForVar(varName);
        VarSymbolEntry *varEntry = varTable->findSymbol(varName);
        llvm::Value* leftPtr = varEntry->LLVMValue;
        if (leftSideVarRef->isArray()) {
            leftPtr = createGEPInstruction(leftSideVarRef, varEntry->LLVMValue);
        }

        Expr* rightSideExpr = (Expr*)expr->getChild(1);
        llvm::Value *rightSideExprLlvmValue = getLlvmValueForExpr(rightSideExpr);
        //  the store instruction does not return a value and hence does not need a virtual register
        // to hold its value, we do not need to use its return value. I just ignored its return value.
        TheBuilder->CreateStore(rightSideExprLlvmValue, leftPtr);
        // setLlvmValueForExpr(expr, assignmentExprLlvmValue);
    }

    void IRGenerator::visitIntLiteralExpr(IntLiteralExpr *literal) {
        llvm::Constant *intLlvmValue = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), literal->value(), true);
        setLlvmValueForExpr((Expr *)literal, (llvm::Value *)intLlvmValue);
    }

    void IRGenerator::visitBoolLiteralExpr(BoolLiteralExpr *literal) {
        llvm::Constant *boolLlvmValue = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), literal->value(), true);
        setLlvmValueForExpr((Expr *)literal, (llvm::Value *)boolLlvmValue);
    }

    void IRGenerator::visitScope(ScopeStatement *stmt) {

        for (size_t i = 0; i < stmt->numChildren(); i++) {
            auto child = stmt->getChild(i);
            if (child){
                // if there are childern after a "return" stmt, we do not need to visit them
                // since they are considered "unreachable" code. So if you see the block
                // has already terminated with a return, do not visit its childern.
                if (!TheBuilder->GetInsertBlock()->getTerminator()) {
                    child->accept(this);
                }
            }
        }
    }
}