//
// Created by Fan Long on 2020/12/6.
//

#ifndef MINICC_IRGENERATOR_H
#define MINICC_IRGENERATOR_H

//add more header files if your want
#include "ASTVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "Program.h"

namespace minicc {

    class IRGenerator : public ASTVisitor {

        std::unique_ptr<llvm::LLVMContext> TheContext;
        std::unique_ptr<llvm::Module> TheModule;
        std::unique_ptr<llvm::IRBuilder<>> TheBuilder;
        std::string ModuleName;

        //add your variables and member functions

        // The map of Expr* to llvm values holds the llvm registers for us
        // while we traverse the AST. We can store/load registers values to/from it.
        std::map<Expr*, llvm::Value*> ExprValueMap;

        // The stack of "out" basic blocks used in "break" stmts
        std::vector<llvm::BasicBlock*> loopsOutBasicBlockStack;

       public:
        explicit IRGenerator(const std::string moduleName) : ASTVisitor(), TheModule(), TheBuilder(), ModuleName(moduleName) {
            TheContext = std::make_unique<llvm::LLVMContext>();
        }

        std::unique_ptr<llvm::Module> getModule() {
            TheBuilder.reset();
            return std::move(TheModule);
        }

        llvm::Value* getLlvmValueForExpr(Expr* expr) {
            auto it = ExprValueMap.find(expr);
            assert(it != ExprValueMap.end());
            return it->second;
        }

        void setLlvmValueForExpr(Expr* expr, llvm::Value* value) {
            ExprValueMap.insert(std::make_pair(expr, value));
        }

        llvm::Type *getLlvmValueOfType(minicc::Type type) {
            if (type.primitiveType() == Type::Bool) {
                return llvm::Type::getInt1Ty(*TheContext);
            } else if (type.primitiveType() == Type::Int) {
                return llvm::Type::getInt32Ty(*TheContext);
            } else {
                return llvm::Type::getVoidTy(*TheContext);
            }
        }

        llvm::Value *createGEPInstruction(VarReference *varRef, llvm::Value *ptrValue);

        llvm::Value* translateLogicalBinaryExpr(BinaryExpr *expr);

        void setParametersLlvmValues(FuncDeclaration *func, VarSymbolTable *varTable);

        void allocateParameters(FuncDeclaration *func, llvm::Function *llvmFunc);

        void allocateLocalVars(VarSymbolTable *varTable, std::vector<std::string> varNamesVector);

        void allocateGlobalVars(VarSymbolTable *varTable, std::vector<std::string> varNamesVector);

        void visitProgram(Program* prog) override;

        void visitVarDecl(VarDeclaration *decl) override;

        void visitFuncDecl(FuncDeclaration *func) override;

        void visitIfStmt(IfStatement *stmt) override;

        void visitForStmt(ForStatement *stmt) override;

        void visitWhileStmt(WhileStatement *stmt) override;

        void visitReturnStmt(ReturnStatement *stmt) override;

        void visitBreakStmt(BreakStatement *stmt) override;

        void visitScope(ScopeStatement *stmt) override;

        void visitUnaryExpr(UnaryExpr *expr) override;

        void visitBinaryExpr(BinaryExpr *expr) override;

        void visitCallExpr(CallExpr *expr) override;

        void visitVarExpr(VarExpr *expr) override;

        // void visitVarRef(VarReference *var) override;

        void visitAssignmentExpr(AssignmentExpr *expr) override;

        void visitIntLiteralExpr(IntLiteralExpr *literal) override;

        void visitBoolLiteralExpr(BoolLiteralExpr *literal) override;
    };
}

#endif //MINICC_IRGENERATOR_H
