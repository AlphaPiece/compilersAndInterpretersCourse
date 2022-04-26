grammar MiniC;

@header {
#include <vector>
#include "Program.h"
#include "Declarations.h"
#include "Statements.h"
#include "Exprs.h"
#include "Terms.h"
}

/*Add your grammar rules in Assignment 2*/
/*Then add compiler actions in Assignment 3*/
prog returns [minicc::Program *val]
    /*You may need init in Assignment 3*/
    @init {
	    $val = new minicc::Program();
    }
    :
    (preamble {
        $val->setSyslibFlag(true);
    } | )
    (decl {
        $val->addChild($decl.d);
    })*
    EOF
    {
        $val->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
preamble:  '#include' '"minicio.h"';
decl returns [minicc::Declaration *d]
    :
    vardecl {
        $d = $vardecl.vd;
        $d->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    |   rettype funcname '(' parameters ')' scope
        {
            minicc::FuncDeclaration *fd = new minicc::FuncDeclaration();
            fd->setHasBody(true);
            fd->addChild($rettype.rt);
            fd->addChild($funcname.fName);
            for(auto it = $parameters.params.begin() ; it != $parameters.params.end(); ++it) {
                fd->addChild(*it);
            }
            fd->addChild($scope.ss);
            $d = fd;
            $d->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   rettype funcname '(' parameters ')' ';' {
            minicc::FuncDeclaration *fd = new minicc::FuncDeclaration();
            fd->addChild($rettype.rt);
            fd->addChild($funcname.fName);
            for(auto it = $parameters.params.begin() ; it != $parameters.params.end(); ++it) {
                fd->addChild(*it);
            $d = fd;
        }
        $d->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
vardecl returns [minicc::VarDeclaration *vd]
    :
    vartype varlist ';' {
        $vd = new minicc::VarDeclaration();
        $vd->addChild($vartype.t);
	    for(auto it = $varlist.vLst.begin() ; it != $varlist.vLst.end(); ++it) {
            $vd->addChild(*it);
        }
        $vd->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
scope returns [minicc::ScopeStatement *ss]
/*You may need init in Assignment 3*/
@init {
        size_t numVarExprs = 0;
        $ss = new minicc::ScopeStatement();
    }
    :
    '{' (vardecl {
            $ss->addChild($vardecl.vd);
            numVarExprs = numVarExprs + $vardecl.vd->numVarReferences();
        }
    )* (stmt {
        $ss->addChild($stmt.s);
    })*'}'
    {
        $ss->setNumVarDecl(numVarExprs);
        $ss->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
stmt returns [minicc::Statement *s]
    :
    expr ';' {
        $s = new minicc::ExprStatement();
        ((minicc::ExprStatement*)$s)->addChild($expr.e);
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'if' '(' expr ')' stmt {
        $s = new minicc::IfStatement();
        ((minicc::IfStatement*)$s)->addChild($expr.e);
        ((minicc::IfStatement*)$s)->addChild($stmt.s);
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'if' '(' expr ')' s1=stmt 'else' s2=stmt  {
        $s = new minicc::IfStatement();
        ((minicc::IfStatement*)$s)->addChild($expr.e);
        ((minicc::IfStatement*)$s)->addChild($s1.s);
        ((minicc::IfStatement*)$s)->addChild($s2.s);
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'for' '(' e1=expropt ';' e2=expropt ';' e3=expropt ')' stmt {
	    $s = new minicc::ForStatement();
        ((minicc::ForStatement*)$s)->addChild($e1.eo);
        ((minicc::ForStatement*)$s)->addChild($e2.eo);
        ((minicc::ForStatement*)$s)->addChild($e3.eo);
        ((minicc::ForStatement*)$s)->addChild($stmt.s);
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'while' '(' e1=expropt ')' stmt {
        $s = new minicc::WhileStatement();
        ((minicc::WhileStatement*)$s)->addChild($e1.eo);
        ((minicc::WhileStatement*)$s)->addChild($stmt.s);
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'break' ';' {
        $s = new minicc::BreakStatement();
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'return' ';' {
        $s = new minicc::ReturnStatement();
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | 'return' expr ';' {
        $s = new minicc::ReturnStatement();
        ((minicc::ReturnStatement*)$s)->addChild($expr.e);
        $s->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | scope {
        $s = $scope.ss;
    }
    ;
varlist returns [std::vector<minicc::VarReference*> vLst]
    :
    varlistentry {
        $vLst.clear();
        $vLst.push_back($varlistentry.ve);
    }
    |   v1=varlist ',' varlistentry {
            $vLst = $v1.vLst;
            $vLst.push_back($varlistentry.ve);
        }
    ;
varlistentry returns [minicc::VarReference *ve]
    :
    varname {
        $ve = new minicc::VarReference();
        $ve->addChild($varname.varName);
        $ve->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    | varname '[' INT ']' {
            $ve = new minicc::VarReference();
            minicc::ConstantLiteralExpr *n = minicc::ConstantLiteralExpr::fromString($INT.text);
            $ve->addChild($varname.varName);
            $ve->addChild(n);
            n->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
            $ve->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    ;
vartype returns [minicc::TypeReference *t]
    :
	'int' {
        $t = new minicc::TypeReference(Type::Int);
        $t->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |'bool' {
        $t = new minicc::TypeReference(Type::Bool);
        $t->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    ;
rettype returns [minicc::TypeReference *rt]
    :
    'void' {
	        $rt = new minicc::TypeReference(Type::Void);
            $rt->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   vartype {
            $rt = $vartype.t;
            $rt->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    ;
parameters returns [std::vector<minicc::Parameter*> params]
    :
	/* eps */
    {
        $params.clear ();
    }
    |   parameterlist {
            $params = $parameterlist.pLst;
        }
    ;
parameterlist returns [std::vector<minicc::Parameter*> pLst]
    :
    parameterentry {
	        $pLst.clear ();
            $pLst.push_back($parameterentry.p);
        }
    |   p1=parameterlist ',' parameterentry {
	        $pLst = $p1.pLst;
            $pLst.push_back($parameterentry.p);
        }
    ;
parameterentry returns [minicc::Parameter *p]
    :
    vartype parametername {
        $p = new minicc::Parameter();
        $p->addChild($vartype.t);
        $p->addChild($parametername.param);
        $p->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
expropt returns [minicc::Expr *eo]
    :
    expr {
            $eo = $expr.e;
            $eo->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   /* eps */ {
            $eo = nullptr;
        }
    ;
expr returns [minicc::Expr *e, bool compare]
@init { $compare=false;}
    :
    INT {
            $e = minicc::ConstantLiteralExpr::fromString($INT.text);
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   op='-' e1=expr {
            if (typeid(*$e1.e) == typeid(minicc::IntLiteralExpr)) {
                delete $e1.e;
                // check the int to see if it has an out of bound value. Note ConstantLiteralExpr will produce "void" type if the int is out of bound.
                $e1.e = minicc::ConstantLiteralExpr::fromString($e1.text, true);
                $e1.e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine()); 
            }
	        minicc::UnaryExpr *exp = new minicc::UnaryExpr();
	        exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
            $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   e1=expr op=('*'|'/') e2=expr {
            minicc::BinaryExpr *exp = new minicc::BinaryExpr();
            exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
            exp->addChild($e2.e);
            $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   e1=expr op=('+'|'-') e2=expr {
            minicc::BinaryExpr *exp = new minicc::BinaryExpr();
            exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
            exp->addChild($e2.e);
            $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   e1=expr op=('==' | '!=' | '<' | '<=' | '>' | '>=') e2=expr {
            minicc::BinaryExpr *exp = new minicc::BinaryExpr();
            $compare = true;
            exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
            exp->addChild($e2.e);
	        $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   op='!' e1=expr {
            minicc::UnaryExpr *exp= new minicc::UnaryExpr();
            exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
	        $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   e1=expr op='&&' e2=expr {
            minicc::BinaryExpr *exp = new minicc::BinaryExpr();
            exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
            exp->addChild($e2.e);
	        $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   e1=expr op='||' e2=expr {
            minicc::BinaryExpr *exp = new minicc::BinaryExpr();
            exp->setOpcode(minicc::Expr::opcodeFromString($op.text));
            exp->addChild($e1.e);
            exp->addChild($e2.e);
            $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   var '=' expr {
		    minicc::AssignmentExpr *exp = new minicc::AssignmentExpr();
            exp->addChild($var.v);
            exp->addChild($expr.e);
            $e = exp;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   v='true' {
            $e = new minicc::BoolLiteralExpr(true);
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   v='false' {
            $e = new minicc::BoolLiteralExpr(false);
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   '(' expr ')' {
            $e = $expr.e;
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   var {
           $e = new minicc::VarExpr(); // Assuming each VarExpr has a VarReference child
           ((minicc::VarExpr*)$e)->addChild($var.v);
           $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
        }
    |   funcname '(' arguments ')' {
            $e = new minicc::CallExpr();
            ((minicc::CallExpr*)$e)->addChild($funcname.fName);
            for(auto it = $arguments.args.begin() ; it != $arguments.args.end(); ++it) {
                ((minicc::CallExpr*)$e)->addChild(*it);
            }
            $e->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    |   parametername {
	        $e = new minicc::VarExpr();
            minicc::VarReference *tmp = new minicc::VarReference();
            tmp->addChild($parametername.param);
            tmp->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
            ((minicc::VarExpr*)$e)->addChild(tmp);
        }
    ;
var returns [minicc::VarReference *v]
    :
    varname {
        $v = new minicc::VarReference();
        $v->addChild($varname.varName);
        $v->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    |   varname '[' expr ']' {
        $v = new minicc::VarReference();
        $v->addChild($varname.varName);
        $v->addChild($expr.e);
        $v->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
arguments returns [std::vector<minicc::Expr*> args]
    :   /* epsilon */
    {
        $args.clear();
    }
    |   argumentlist {
            $args = $argumentlist.argsLst;
    }
    ;
argumentlist returns [std::vector<minicc::Expr*> argsLst]
    :
    expr {
        $argsLst.clear();
        $argsLst.push_back($expr.e);
    }
    |   a1=argumentlist ',' expr {
	        $argsLst = $a1.argsLst;
            $argsLst.push_back($expr.e);
    }
    ;
varname returns [minicc::Identifier *varName]
    :
	ID {
        $varName = new minicc::Identifier($ID.text);
        $varName->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
funcname returns [minicc::Identifier *fName]
    :
	ID {
        $fName = new minicc::Identifier($ID.text);
        $fName->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;
parametername returns [minicc::Identifier *param]
    :
    ID {
        $param = new minicc::Identifier($ID.text);
	    $param->setSrcLoc($ctx->start->getLine(), $ctx->start->getCharPositionInLine());
    }
    ;

ID:     [a-zA-Z][a-zA-Z0-9_]* ;
INT:    [0] | ([1-9][0-9]*);
WS:     [ \t\r\n]+ -> skip;
COMMENT: '//' (~[\r\n])* -> skip;
