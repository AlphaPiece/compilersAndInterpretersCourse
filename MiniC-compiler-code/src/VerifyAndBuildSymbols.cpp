//
// Created by Fan Long on 2020/12/4.
//

//Add necessary headers you want
#include "VerifyAndBuildSymbols.h"
#include "Declarations.h"
#include "Terms.h"
#include "Types.h"
#include "Exprs.h"
#include "Statements.h"
#include "Program.h"
#include <string>
#include <sstream>
#include <iostream>

namespace minicc {

    /*********************** helper functions *************************/

    void addErrorMessage(std::string msg, ASTNode *node, std::vector<ErrorMessage> &Errors) {
        struct ErrorMessage errorMsg(std::string(msg), node->srcLoc());
        Errors.push_back(errorMsg);
    }

    void insertFunctionDeclaration(FuncDeclaration *funcDecl, FuncSymbolTable *funcTable, std::vector<ErrorMessage> &Errors) {

        std::string funcName = funcDecl->name();
        ScopeStatement *funcBody = nullptr;
        VarSymbolTable *varTable = nullptr;
        if (funcDecl->hasBody()) {
            funcBody = funcDecl->body();
            varTable = funcBody->scopeVarTable();
        }
        Type rType = funcDecl->returnType();
        std::vector<Type> parameterTypesVector;
        for (int t = 0; t < funcDecl->numParameters(); t++) {
            Parameter *param = funcDecl->parameter(t);
            std::string paramName = param->name();
            Type paramType = param->type();
            parameterTypesVector.push_back(param->type());
            if (funcDecl->hasBody()) {
                if (varTable->containsSymbol(paramName)) {
                    // Error - duplicate declaration
                    addErrorMessage("Redefinition of variable/parameter \"" + paramName + "\" in the same scope!", param, Errors);
                } else {
                    // The paramName is not in the var table yet, so add it to var table
                    struct VarSymbolEntry varEntry(paramType);
                    varTable->insertSymbol(std::make_pair(paramName, varEntry));
                }
            }
        }
        struct FuncSymbolEntry funcEntry(rType, parameterTypesVector, funcDecl->hasBody());
        FuncSymbolEntry *prevFuncEntry = funcTable->findSymbol(funcName);
        if (prevFuncEntry) {
            prevFuncEntry->HasBody = true;
        } else {
            funcTable->insertSymbol(std::make_pair(funcName, funcEntry));
        }
    }

    void insertVariableDeclaration(VarReference *varRef, std::string &varName, Type varType, VarSymbolTable *scopeVarTable, std::vector<ErrorMessage> &Errors) {
        if (scopeVarTable->containsSymbol(varName)) {
            // Error - duplicate declaration
            addErrorMessage("Redefinition of variable/parameter \"" + varName + "\" in the same scope!", varRef, Errors);
        } else {
            // The varName is not in the var table yet, so add it to var table
            struct VarSymbolEntry varEntry(varType);
            // check if the var is an array and set its bound
            if (varRef->isArray()) {
                int index = ((IntLiteralExpr *)varRef->indexExpr())->value();
                varEntry.VarType.setIsArray(index);
            }
            scopeVarTable->insertSymbol(std::make_pair(varName, varEntry));
        }
    }

    bool functionDeclIsConsistent(FuncDeclaration *func, struct FuncSymbolEntry *prevFuncEntry, std::vector<ErrorMessage> &Errors) {
        bool result = true;
        std::vector<Type> prevParameterVector = prevFuncEntry->ParameterTypes;
        Type prevReturnType = prevFuncEntry->ReturnType;
        std::string funcName = func->name();

        if (func->returnType() != prevReturnType) {
            // Error
            addErrorMessage("Definition of function \"" + funcName + "()\" with different return type!", func, Errors);

            result = false;
        }

        if (func->numParameters() != prevParameterVector.size()) {
            // Error
            addErrorMessage("Definition of function \"" + funcName + "()\" with different number of parameters!", func, Errors);

            result = false;
        }

        for (int i = 0; i < func->numParameters(); i++) {
            Parameter *param = func->parameter(i);
            if (param->type() != prevParameterVector[i]) {
                // Error
                addErrorMessage("Definition of function \"" +
                    funcName + "()\" with different parameter type at position "
                    + std::to_string(i) + "!", func, Errors);

                result = false;
            }
        }
        return result;
    }

    // I did not use this helper anywhere.
    bool functionParametersAreDistinct(FuncDeclaration *func, std::vector<ErrorMessage> &Errors) {
        bool result = true;
        for (int i = 0; i < func->numParameters(); i++) {
            Parameter *param = func->parameter(i);
            for (int t = 0; t < func->numParameters(); t++) {
                if (i != t) {
                    Parameter *p = func->parameter(t);
                    if (p->name() == param->name()) {
                        // Error
                        addErrorMessage("Redefinition of variable/parameter \"" + param->name() + "\" in the same scope!", func, Errors);
                        result = false;
                    }
                }
            }
        }
        return result;
    }

    bool functionDeclReturnIsValid (FuncDeclaration *func, std::vector<ErrorMessage> &Errors) {

        ScopeStatement *bodyStatement = func->body();
        size_t lastChildIndex = bodyStatement->numChildren() - 1;
        ASTNode *lastChild = bodyStatement->getChild(lastChildIndex);
        if (func->returnType() != Type(Type::Void)) {
            // bool result = lastChild->isReturn();
            if (!lastChild->isReturn()) {
                if (!lastChild->isIfStatement()) {
                    // Error
                    addErrorMessage("The function \"" + func->name() + "()\" need to return a value at its end!", func, Errors);
                    return false;
                } else {
                    if (!((IfStatement *)lastChild)->hasElse()) {  // the if stmt does not have an else and also the last child of this function body was not a return, so this is an error
                        // Error
                        addErrorMessage("The function \"" + func->name() + "()\" need to return a value at its end!", func, Errors);
                        return false;
                    } else {
                        // make sure both of if and else parts return a value
                        Statement *thenStmt = ((IfStatement *)lastChild)->thenStmt();
                        Statement *elseStmt = ((IfStatement *)lastChild)->elseStmt();
                        ASTNode *thenStmtLastChild = thenStmt->getChild(thenStmt->numChildren() - 1);
                        ASTNode *elseStmtLastChild = elseStmt->getChild(elseStmt->numChildren() - 1);

                        // Since the last stmt of the function body was not a return, both of the then and else stmts should return sth
                        if (!(thenStmtLastChild->isReturn() && elseStmtLastChild->isReturn())) {
                            // Error
                            addErrorMessage("The function \"" + func->name() + "()\" need to return a value at its end!", func, Errors);
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    bool returnStmtIsValid (ReturnStatement *rStmt, std::vector<ErrorMessage> &Errors) {

        bool result = true;
        FuncDeclaration* func = rStmt->getParentFunction();

        if (func->returnType() != Type(Type::Void)) {
            if (rStmt->hasReturnExpr()) {
                Type stmtReturnType = rStmt->returnExpr()->exprType();
                if (stmtReturnType != func->returnType()) {
                    addErrorMessage("Function has return type \"" + func->returnType().toString() 
                        + "\", but the returned expression has type \"" + stmtReturnType.toString() + "\"!", rStmt, Errors);
                    result = false;
                }
            } else {
                addErrorMessage("Function has non-void return type, but the return statement has no returned expression!", rStmt, Errors);
                result = false;
            }
        } else if (rStmt->hasReturnExpr()) {
            // Function is void but rStmt returns sth
            addErrorMessage("Function has void return type, but the return statement has a returned expression!", rStmt, Errors);
            result = false;
        }
        return result;
    }

    /****************************************************************************/

    void VerifyAndBuildSymbols::visitASTNode(ASTNode *node) {
        // set root of the node before visiting children
        if (node->isProgram()) {
            this->setVisitingProgram((minicc::Program *) node);
        }
        // Set the root of the program for all of the visiting nodes
        // This is because since only the program node has the function symbol table
        // and hence if any of the child nodes want to access the function symbol table
        // they should access the program node
        node->setRoot((minicc::Program *)(this->getVisitingProgram()));
        ASTVisitor::visitASTNode(node);
    }

    void VerifyAndBuildSymbols::visitProgram(Program *prog) {

        FuncSymbolTable *funcTable = prog->funcTable();
        VarSymbolTable *varTable = prog->scopeVarTable();

        // Add the lib functions to the function table
        if (prog->syslibFlag()) {
            // Manually populate the lib functions to the function symbol table
            /*
                int getint();

                void putint(int v);

                void putnewline();

            */
            std::vector<Type> rTypesVector;
            std::vector<std::string> funcNamesVector;
            std::vector<std::vector<Type>> parameterTypesVector;

            rTypesVector.push_back(Type(minicc::Type::Int));
            rTypesVector.push_back(Type(minicc::Type::Void));
            rTypesVector.push_back(Type(minicc::Type::Void));
            funcNamesVector.push_back(std::string("getint"));
            funcNamesVector.push_back(std::string("putint"));
            funcNamesVector.push_back(std::string("putnewline"));
            std::vector<Type> pVector1;
            std::vector<Type> pVector2;
            pVector2.push_back(Type(minicc::Type::Int));
            std::vector<Type> pVector3;
            parameterTypesVector.push_back(pVector1);
            parameterTypesVector.push_back(pVector2);
            parameterTypesVector.push_back(pVector3);

            for(int i = 0; i < 3; i++) {
                struct FuncSymbolEntry funcEntry(rTypesVector[i], parameterTypesVector[i], true);
                // insert it into the function table
                funcTable->insertSymbol(std::make_pair(funcNamesVector[i], funcEntry));
            }
        }
        // now loop over the children
        ASTVisitor::visitProgram(prog);
    }

    void VerifyAndBuildSymbols::visitVarDecl(VarDeclaration *decl) {
        
        // loop over the children
        ASTVisitor::visitVarDecl(decl);

        // Check that same variable cannot be declared twice in the same scope
        Type varType = Type(decl->declType());
        // check to see if the var is already declared in the closest scope
        ASTNode *currentScope = decl->getParentScope();

        // currentScope should be not be null, because we allow a declaration either
        // in the program scope or a scope statement otherwise we would have parse errors
        // earlier than this phase. So the currentScope should at extreme case the prog node itself.

        VarSymbolTable *scopeVarTable = currentScope->scopeVarTable();
        for(int i = 0; i < decl->numVarReferences(); i++) {
            VarReference *varRef = decl->varReference(i);
            std::string varName = (varRef->identifier())->name();
            insertVariableDeclaration(varRef, varName, varType, scopeVarTable, this->Errors);
        }
    }

    void VerifyAndBuildSymbols::visitFuncDecl(FuncDeclaration *func) {

        // Check return type of the function does not match with each other
        // Check number of parameters should match with each other
        // Check each parameter type should match with each other
        // Check there should be only one definition of the function
        // Check parameters cannot have the same name
        // Check the last statement a function body must be return if the return type is not void

        std::string funcName = func->name();
        FuncSymbolTable *programFuncTable = (this->getVisitingProgram())->funcTable();
        FuncSymbolEntry *prevFuncEntry = programFuncTable->findSymbol(funcName);
        bool result = true;

        if (func->hasBody()) {
            // Check the last statement a function body must be return if the return type is not void
            result = result && functionDeclReturnIsValid(func, this->Errors);
        }
        if (prevFuncEntry != nullptr) {  // function name already declared
            // check that these two func decl should match
            result = result && functionDeclIsConsistent(func, prevFuncEntry, this->Errors);
            if (func->hasBody() && prevFuncEntry->HasBody) {
                addErrorMessage("Redefinition of function \"" + func->name() + "()\"!", func, this->Errors);
                result = false;
            } else if (result && func->hasBody() && !prevFuncEntry->HasBody) {
                insertFunctionDeclaration(func, programFuncTable, this->Errors);
            }
        } else {
            insertFunctionDeclaration(func, programFuncTable, this->Errors);
        }

        // now loop over the children
        ASTVisitor::visitFuncDecl(func);
    }

    void VerifyAndBuildSymbols::visitIfStmt(IfStatement *stmt) {

        // loop over the children
        ASTVisitor::visitIfStmt(stmt);

        // Check the conditional expression must have bool type
        Expr *condExpr = stmt->condExpr();
        if (!(condExpr->exprType().isBool())) {
            // Error
            addErrorMessage("Conditional expression in if statement has non-bool type!", condExpr, this->Errors);
        }
    }

    void VerifyAndBuildSymbols::visitForStmt(ForStatement *stmt) {

        // loop over the children
        ASTVisitor::visitForStmt(stmt);

        // Check the second expression in for must be either null or bool type
        Expr* condExpr = stmt->condExpr();
        if (!(condExpr->exprType().isBool())) {
            // Error
            addErrorMessage("Conditional expression in for statement has non-bool type!", condExpr, this->Errors);
        }
    }

    void VerifyAndBuildSymbols::visitWhileStmt(WhileStatement *stmt) {

        // loop over the children
        ASTVisitor::visitWhileStmt(stmt);

        // Check the conditional expression must have bool type
        Expr *condExpr = stmt->condExpr();
        if (!(condExpr->exprType().isBool())) {
            // Error
            addErrorMessage("Conditional expression in while statement has non-bool type!", stmt, this->Errors);
        }
    }

    void VerifyAndBuildSymbols::visitReturnStmt(ReturnStatement *stmt) {

        // loop over the children
        ASTVisitor::visitReturnStmt(stmt);

        // Check void function must have no expression to return
        // Check Non-Void function must have an expression to return
        // Check the return type and the returned expression type must match
        returnStmtIsValid(stmt, Errors);
    }

    void VerifyAndBuildSymbols::visitBreakStmt(BreakStatement *stmt) {
        
        // Check Break statement must appear inside a for or a while statement
        ForStatement* forStmt = stmt->getParentForStatement();
        WhileStatement *whileStmt = stmt->getParentWhileStatement();
        if (forStmt == nullptr && whileStmt == nullptr) {
            // Error
            addErrorMessage("Break statement must appear inside a for statement!", stmt, this->Errors);
        }

        // now loop over the children
        ASTVisitor::visitBreakStmt(stmt);
    }

    void VerifyAndBuildSymbols::visitUnaryExpr(UnaryExpr *expr) {

        // loop over the children
        ASTVisitor::visitUnaryExpr(expr);
        Expr *e = (Expr*)expr->getChild(0);

        // Check Negate opcode must have int operand!
        // Check Not opcode must have bool operand

        Expr::ExprOpcode opCode = expr->opcode();

        if (Expr::opcodeToString(opCode) == "-") {            
            if (!(e->exprType().isInt())) {
                // Error
                addErrorMessage("Negate \"-\" opcode must have int operand!", expr, this->Errors);
            }
        } else if (Expr::opcodeToString(opCode) == "!") {
            // Error
            if (!(e->exprType().isBool())) {
                addErrorMessage("Not \"!\" opcode must have bool operand!", expr, this->Errors);
            }
        }
        expr->setExprType(e->exprType());
    }

    void VerifyAndBuildSymbols::visitBinaryExpr(BinaryExpr *expr) {

        // loop over the children
        ASTVisitor::visitBinaryExpr(expr);

        // Check that for logical opcode, both operand need to be bool
        // Check that for equal and not equal opcode, both operand need to be the same primitive types
        // Check that for arithmetic and other comparison operand, both operand need to be int

        Expr::ExprOpcode opCode = expr->opcode();
        Expr *e1 = (Expr *)(expr->getChild(0));
        Expr *e2 = (Expr *)(expr->getChild(1));

        if (Expr::opcodeToString(opCode) == "&&" || Expr::opcodeToString(opCode) == "||") {
            if (!(e1->exprType().isBool() && e2->exprType().isBool())) {
                // Error
                addErrorMessage("\"&&\"/\"||\" opcode must have bool operand!", expr, this->Errors);
            } else {
                expr->setExprType(Type(minicc::Type::Bool));
            }
        }

        if (Expr::opcodeToString(opCode) == "==" || Expr::opcodeToString(opCode) == "!=") {
            if (!
                ((e1->exprType().isInt() && e2->exprType().isInt()) 
                || (e1->exprType().isBool() && e2->exprType().isBool()))
                ) {
                // Error
                addErrorMessage("\"==\"/\"!=\" opcode must have same primitive type operand!", expr, this->Errors);
            } else {
                expr->setExprType(Type(minicc::Type::Bool));  // e1 and e2 are both either bool or int
            }
        }

        bool isNumberOperator = false;
        bool isComparisonOperator = false;

        if ( Expr::opcodeToString(opCode) == "+" 
            || Expr::opcodeToString(opCode) == "-"
            || Expr::opcodeToString(opCode) == "*"
            || Expr::opcodeToString(opCode) == "/") {
            
            isNumberOperator = true;
        }

        if (Expr::opcodeToString(opCode) == "<"
            || Expr::opcodeToString(opCode) == "<="
            || Expr::opcodeToString(opCode) == ">"
            || Expr::opcodeToString(opCode) == ">=") {
            
            isComparisonOperator = true;
        }

        if (isNumberOperator || isComparisonOperator) {
            if (!(e1->exprType().isInt() && e2->exprType().isInt())) {
                // Error
                addErrorMessage("\"" + minicc::Expr::opcodeToString(opCode) + "\" opcode must have int type operand!", expr, this->Errors);
            } else {
                if (isNumberOperator) {
                    expr->setExprType(Type(minicc::Type::Int));
                } else { // it is a isComparisonOperator
                    expr->setExprType(Type(minicc::Type::Bool));
                }
            }
        }
    }

    void VerifyAndBuildSymbols::visitCallExpr(CallExpr *expr) {

        // loop over the children
        ASTVisitor::visitCallExpr(expr);

        // Check Call undeclared function
        // Check the number of arguments must match the number of parameters
        // Check the type of each parameter must match the argument

        std::string callee = expr->callee()->name();
        FuncSymbolTable *funcTable = (expr->root())->funcTable();
        FuncSymbolEntry *funcEntry = funcTable->findSymbol(callee);
        if (funcEntry == nullptr || !funcEntry->HasBody) {
            // Error
            addErrorMessage("Function " + callee + "() is not declared before use!", expr, this->Errors);
            return;
        } else {
            // set the function call's expr type
            expr->setExprType(funcEntry->ReturnType);
        }

        // at this point we know the function is defined, but we should check if it is
        // called with correct args or not
        if (expr->numArgs() != funcEntry->ParameterTypes.size()) {
            // Error
            addErrorMessage(
                "Function " + callee + "() is declared with " + std::to_string(funcEntry->ParameterTypes.size()) + " parameters but called with " + std::to_string(expr->numArgs()) + " arguments!",
                expr, this->Errors);
        } else {
            for (int i = 0; i < funcEntry->ParameterTypes.size(); i++) {
                Type paramType = funcEntry->ParameterTypes[i];
                if (paramType != expr->arg(i)->exprType()) {
                    // Error
                    addErrorMessage("Function " + callee + "() does not match the type of the call argument at position " + std::to_string(i) + "!", expr, this->Errors);
                }
            }
        }
    }

    static Type verifyVarReference(std::vector<ErrorMessage> &Errors, Expr* expr, VarReference *ref) {

        // Check the vairable which is reference must be declared before
        // Check index expression must have int type
        // Check variable must be declared as an array for indexing

        Type t = Type(minicc::Type::Void);

        std::string varName = ref->identifier()->name();
        VarSymbolTable *varTable = ref->locateDeclaringTableForVar(varName);
        if (varTable == nullptr) {
            // Error
            addErrorMessage("Variable " + varName + " is not declared before use!", ref, Errors);
        } else {
            struct VarSymbolEntry *varEntry = varTable->findSymbol(varName);
            t = varEntry->VarType;

            bool hasIndexExpr = ref->isArray();
            if (t.arrayBound() > 0) { // the ref is declared as an array
                t = varEntry->VarType.getIndexedType();
                if (!hasIndexExpr || !(ref->indexExpr()->exprType().isInt())) {
                    // Error
                    addErrorMessage("Array index expressions must have int operand!", expr, Errors);
                }
                // We could probably check the array index out of bound
                // by making sure ref->indexExpr()->exprType().isInt() < t.arrayBound()
                // here but the assignment requirement has not asked us to, so I skipped it for now.
            } else {
                // the ref is declared to be a non-array but has been indexed
                if (hasIndexExpr) {
                    addErrorMessage("Indexing an non-array variable!", ref, Errors);
                }
            }
        }

        return t;
    }

    void VerifyAndBuildSymbols::visitVarExpr(VarExpr *expr) {

        // now loop over the children
        ASTVisitor::visitVarExpr(expr);
        // invoke verifyVarReference to verify
        VarReference *varRef = (VarReference *)(expr->getChild(0));
        Type varType = verifyVarReference(Errors, (Expr *)expr, varRef);
        expr->setExprType(varType);
       
    }

    void VerifyAndBuildSymbols::visitAssignmentExpr(AssignmentExpr *expr) {
        // invoke verifyVarReference to verify
        // Also, check var and assigned expression must have the same type

        // loop over the children
        ASTVisitor::visitAssignmentExpr(expr);

        VarReference *varRef = (VarReference *)(expr->getChild(0));
        Expr *e = (Expr *)(expr->getChild(1));

        // The right hand side expr e will be checked when I go over the children of
        // this AssignmentExpr. So only check the "varRef" here
        Type varType = verifyVarReference(Errors, expr, varRef);

        if (varType != e->exprType()) {
            // Error
            addErrorMessage("Variable and the assignment expression do not have the same type!", expr, this->Errors);
        }
        expr->setExprType(varType);
    }

    void VerifyAndBuildSymbols::visitIntLiteralExpr(IntLiteralExpr *literal) {

        // Check Integer literal must be inside the range of int

        // Parser in file `Expr.cpp` catches the INT out of bounds and makes a int literal
        // of type void out of them. So if the literal here is not of tyoe int, it means
        // it has been out of bound.
        if (!literal->exprType().isInt()) { 
            // Error
            addErrorMessage("Integer literal must be inside the range of int!", literal, this->Errors);
        } 
        literal->setExprType(minicc::Type(minicc::Type::Int));
        
    }

    void VerifyAndBuildSymbols::visitBoolLiteralExpr(BoolLiteralExpr *literal) {

        literal->setExprType(minicc::Type(minicc::Type::Bool));
    }

    //print collected error messages
    std::string VerifyAndBuildSymbols::genErrorMessages() {
        std::stringbuf buf;
        std::ostream os(&buf);

        for (size_t i = 0; i < Errors.size(); i++) {
            os << Errors[i].Msg << " (" << Errors[i].SrcLoc.Line << ":" << Errors[i].SrcLoc.Row << ")\n";
        }

        return buf.str();
    }

}