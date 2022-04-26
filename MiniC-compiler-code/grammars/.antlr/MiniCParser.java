// Generated from /Users/amir/Desktop/CSC488/MiniC/MiniC-compiler-code/grammars/MiniC.g4 by ANTLR 4.8

#include <vector>
#include "Program.h"
#include "Declarations.h"
#include "Statements.h"
#include "Exprs.h"
#include "Terms.h"

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class MiniCParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, T__25=26, T__26=27, T__27=28, T__28=29, T__29=30, T__30=31, 
		T__31=32, T__32=33, T__33=34, T__34=35, ID=36, INT=37, WS=38, COMMENT=39;
	public static final int
		RULE_prog = 0, RULE_preamble = 1, RULE_decl = 2, RULE_vardecl = 3, RULE_scope = 4, 
		RULE_stmt = 5, RULE_varlist = 6, RULE_varlistentry = 7, RULE_vartype = 8, 
		RULE_rettype = 9, RULE_parameters = 10, RULE_parameterlist = 11, RULE_parameterentry = 12, 
		RULE_expropt = 13, RULE_expr = 14, RULE_var = 15, RULE_arguments = 16, 
		RULE_argumentlist = 17, RULE_varname = 18, RULE_funcname = 19, RULE_parametername = 20;
	private static String[] makeRuleNames() {
		return new String[] {
			"prog", "preamble", "decl", "vardecl", "scope", "stmt", "varlist", "varlistentry", 
			"vartype", "rettype", "parameters", "parameterlist", "parameterentry", 
			"expropt", "expr", "var", "arguments", "argumentlist", "varname", "funcname", 
			"parametername"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'#include'", "'\"minicio.h\"'", "'('", "')'", "';'", "'{'", "'}'", 
			"'if'", "'else'", "'for'", "'while'", "'break'", "'return'", "','", "'['", 
			"']'", "'int'", "'bool'", "'void'", "'-'", "'*'", "'/'", "'+'", "'=='", 
			"'!='", "'<'", "'<='", "'>'", "'>='", "'!'", "'&&'", "'||'", "'='", "'true'", 
			"'false'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			"ID", "INT", "WS", "COMMENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "MiniC.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public MiniCParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class ProgContext extends ParserRuleContext {
		public minicc::Program * val;
		public DeclContext decl;
		public TerminalNode EOF() { return getToken(MiniCParser.EOF, 0); }
		public PreambleContext preamble() {
			return getRuleContext(PreambleContext.class,0);
		}
		public List<DeclContext> decl() {
			return getRuleContexts(DeclContext.class);
		}
		public DeclContext decl(int i) {
			return getRuleContext(DeclContext.class,i);
		}
		public ProgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prog; }
	}

	public final ProgContext prog() throws RecognitionException {
		ProgContext _localctx = new ProgContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_prog);

			    ((ProgContext)_localctx).val =  new minicc::Program();
		    
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(46);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__0:
				{
				setState(42);
				preamble();

				        _localctx.val->setSyslibFlag(true);
				    
				}
				break;
			case EOF:
			case T__16:
			case T__17:
			case T__18:
				{
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(53);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__16) | (1L << T__17) | (1L << T__18))) != 0)) {
				{
				{
				setState(48);
				((ProgContext)_localctx).decl = decl();

				        _localctx.val->addChild(((ProgContext)_localctx).decl.d);
				    
				}
				}
				setState(55);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(56);
			match(EOF);

			        _localctx.val->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PreambleContext extends ParserRuleContext {
		public PreambleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_preamble; }
	}

	public final PreambleContext preamble() throws RecognitionException {
		PreambleContext _localctx = new PreambleContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_preamble);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			match(T__0);
			setState(60);
			match(T__1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeclContext extends ParserRuleContext {
		public minicc::Declaration * d;
		public VardeclContext vardecl;
		public RettypeContext rettype;
		public FuncnameContext funcname;
		public ParametersContext parameters;
		public ScopeContext scope;
		public VardeclContext vardecl() {
			return getRuleContext(VardeclContext.class,0);
		}
		public RettypeContext rettype() {
			return getRuleContext(RettypeContext.class,0);
		}
		public FuncnameContext funcname() {
			return getRuleContext(FuncnameContext.class,0);
		}
		public ParametersContext parameters() {
			return getRuleContext(ParametersContext.class,0);
		}
		public ScopeContext scope() {
			return getRuleContext(ScopeContext.class,0);
		}
		public DeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decl; }
	}

	public final DeclContext decl() throws RecognitionException {
		DeclContext _localctx = new DeclContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_decl);
		try {
			setState(81);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,2,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(62);
				((DeclContext)_localctx).vardecl = vardecl();

				        ((DeclContext)_localctx).d =  ((DeclContext)_localctx).vardecl.vd;
				        _localctx.d->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(65);
				((DeclContext)_localctx).rettype = rettype();
				setState(66);
				((DeclContext)_localctx).funcname = funcname();
				setState(67);
				match(T__2);
				setState(68);
				((DeclContext)_localctx).parameters = parameters();
				setState(69);
				match(T__3);
				setState(70);
				((DeclContext)_localctx).scope = scope();

				            minicc::FuncDeclaration *fd = new minicc::FuncDeclaration();
				            fd->setHasBody(true);
				            fd->addChild(((DeclContext)_localctx).rettype.rt);
				            fd->addChild(((DeclContext)_localctx).funcname.fName);
				            for(auto it = ((DeclContext)_localctx).parameters.params.begin() ; it != ((DeclContext)_localctx).parameters.params.end(); ++it) {
				                fd->addChild(*it);
				            }
				            fd->addChild(((DeclContext)_localctx).scope.ss);
				            ((DeclContext)_localctx).d =  fd;
				            _localctx.d->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(73);
				((DeclContext)_localctx).rettype = rettype();
				setState(74);
				((DeclContext)_localctx).funcname = funcname();
				setState(75);
				match(T__2);
				setState(76);
				((DeclContext)_localctx).parameters = parameters();
				setState(77);
				match(T__3);
				setState(78);
				match(T__4);

				            minicc::FuncDeclaration *fd = new minicc::FuncDeclaration();
				            fd->addChild(((DeclContext)_localctx).rettype.rt);
				            fd->addChild(((DeclContext)_localctx).funcname.fName);
				            for(auto it = ((DeclContext)_localctx).parameters.params.begin() ; it != ((DeclContext)_localctx).parameters.params.end(); ++it) {
				                fd->addChild(*it);
				            ((DeclContext)_localctx).d =  fd;
				        }
				        _localctx.d->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VardeclContext extends ParserRuleContext {
		public minicc::VarDeclaration * vd;
		public VartypeContext vartype;
		public VarlistContext varlist;
		public VartypeContext vartype() {
			return getRuleContext(VartypeContext.class,0);
		}
		public VarlistContext varlist() {
			return getRuleContext(VarlistContext.class,0);
		}
		public VardeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vardecl; }
	}

	public final VardeclContext vardecl() throws RecognitionException {
		VardeclContext _localctx = new VardeclContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_vardecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(83);
			((VardeclContext)_localctx).vartype = vartype();
			setState(84);
			((VardeclContext)_localctx).varlist = varlist(0);
			setState(85);
			match(T__4);

			        ((VardeclContext)_localctx).vd =  new minicc::VarDeclaration();
			        _localctx.vd->addChild(((VardeclContext)_localctx).vartype.t);
				    for(auto it = ((VardeclContext)_localctx).varlist.vLst.begin() ; it != ((VardeclContext)_localctx).varlist.vLst.end(); ++it) {
			            _localctx.vd->addChild(*it);
			        }
			        _localctx.vd->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ScopeContext extends ParserRuleContext {
		public minicc::ScopeStatement * ss;
		public VardeclContext vardecl;
		public StmtContext stmt;
		public List<VardeclContext> vardecl() {
			return getRuleContexts(VardeclContext.class);
		}
		public VardeclContext vardecl(int i) {
			return getRuleContext(VardeclContext.class,i);
		}
		public List<StmtContext> stmt() {
			return getRuleContexts(StmtContext.class);
		}
		public StmtContext stmt(int i) {
			return getRuleContext(StmtContext.class,i);
		}
		public ScopeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_scope; }
	}

	public final ScopeContext scope() throws RecognitionException {
		ScopeContext _localctx = new ScopeContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_scope);

		        size_t numVarExprs = 0;
		        ((ScopeContext)_localctx).ss =  new minicc::ScopeStatement();
		    
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(88);
			match(T__5);
			setState(94);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__16 || _la==T__17) {
				{
				{
				setState(89);
				((ScopeContext)_localctx).vardecl = vardecl();

				            _localctx.ss->addChild(((ScopeContext)_localctx).vardecl.vd);
				            numVarExprs = numVarExprs + ((ScopeContext)_localctx).vardecl.vd->numVarReferences();
				        
				}
				}
				setState(96);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(102);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__2) | (1L << T__5) | (1L << T__7) | (1L << T__9) | (1L << T__10) | (1L << T__11) | (1L << T__12) | (1L << T__19) | (1L << T__29) | (1L << T__33) | (1L << T__34) | (1L << ID) | (1L << INT))) != 0)) {
				{
				{
				setState(97);
				((ScopeContext)_localctx).stmt = stmt();

				        _localctx.ss->addChild(((ScopeContext)_localctx).stmt.s);
				    
				}
				}
				setState(104);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(105);
			match(T__6);

			        _localctx.ss->setNumVarDecl(numVarExprs);
			        _localctx.ss->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StmtContext extends ParserRuleContext {
		public minicc::Statement * s;
		public ExprContext expr;
		public StmtContext stmt;
		public StmtContext s1;
		public StmtContext s2;
		public ExproptContext e1;
		public ExproptContext e2;
		public ExproptContext e3;
		public ScopeContext scope;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<StmtContext> stmt() {
			return getRuleContexts(StmtContext.class);
		}
		public StmtContext stmt(int i) {
			return getRuleContext(StmtContext.class,i);
		}
		public List<ExproptContext> expropt() {
			return getRuleContexts(ExproptContext.class);
		}
		public ExproptContext expropt(int i) {
			return getRuleContext(ExproptContext.class,i);
		}
		public ScopeContext scope() {
			return getRuleContext(ScopeContext.class,0);
		}
		public StmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stmt; }
	}

	public final StmtContext stmt() throws RecognitionException {
		StmtContext _localctx = new StmtContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_stmt);
		try {
			setState(160);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(108);
				((StmtContext)_localctx).expr = expr(0);
				setState(109);
				match(T__4);

				        ((StmtContext)_localctx).s =  new minicc::ExprStatement();
				        ((minicc::ExprStatement*)_localctx.s)->addChild(((StmtContext)_localctx).expr.e);
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(112);
				match(T__7);
				setState(113);
				match(T__2);
				setState(114);
				((StmtContext)_localctx).expr = expr(0);
				setState(115);
				match(T__3);
				setState(116);
				((StmtContext)_localctx).stmt = stmt();

				        ((StmtContext)_localctx).s =  new minicc::IfStatement();
				        ((minicc::IfStatement*)_localctx.s)->addChild(((StmtContext)_localctx).expr.e);
				        ((minicc::IfStatement*)_localctx.s)->addChild(((StmtContext)_localctx).stmt.s);
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(119);
				match(T__7);
				setState(120);
				match(T__2);
				setState(121);
				((StmtContext)_localctx).expr = expr(0);
				setState(122);
				match(T__3);
				setState(123);
				((StmtContext)_localctx).s1 = stmt();
				setState(124);
				match(T__8);
				setState(125);
				((StmtContext)_localctx).s2 = stmt();

				        ((StmtContext)_localctx).s =  new minicc::IfStatement();
				        ((minicc::IfStatement*)_localctx.s)->addChild(((StmtContext)_localctx).expr.e);
				        ((minicc::IfStatement*)_localctx.s)->addChild(((StmtContext)_localctx).s1.s);
				        ((minicc::IfStatement*)_localctx.s)->addChild(((StmtContext)_localctx).s2.s);
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(128);
				match(T__9);
				setState(129);
				match(T__2);
				setState(130);
				((StmtContext)_localctx).e1 = expropt();
				setState(131);
				match(T__4);
				setState(132);
				((StmtContext)_localctx).e2 = expropt();
				setState(133);
				match(T__4);
				setState(134);
				((StmtContext)_localctx).e3 = expropt();
				setState(135);
				match(T__3);
				setState(136);
				((StmtContext)_localctx).stmt = stmt();

					    ((StmtContext)_localctx).s =  new minicc::ForStatement();
				        ((minicc::ForStatement*)_localctx.s)->addChild(((StmtContext)_localctx).e1.eo);
				        ((minicc::ForStatement*)_localctx.s)->addChild(((StmtContext)_localctx).e2.eo);
				        ((minicc::ForStatement*)_localctx.s)->addChild(((StmtContext)_localctx).e3.eo);
				        ((minicc::ForStatement*)_localctx.s)->addChild(((StmtContext)_localctx).stmt.s);
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(139);
				match(T__10);
				setState(140);
				match(T__2);
				setState(141);
				((StmtContext)_localctx).e1 = expropt();
				setState(142);
				match(T__3);
				setState(143);
				((StmtContext)_localctx).stmt = stmt();

				        ((StmtContext)_localctx).s =  new minicc::WhileStatement();
				        ((minicc::WhileStatement*)_localctx.s)->addChild(((StmtContext)_localctx).e1.eo);
				        ((minicc::WhileStatement*)_localctx.s)->addChild(((StmtContext)_localctx).stmt.s);
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(146);
				match(T__11);
				setState(147);
				match(T__4);

				        ((StmtContext)_localctx).s =  new minicc::BreakStatement();
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(149);
				match(T__12);
				setState(150);
				match(T__4);

				        ((StmtContext)_localctx).s =  new minicc::ReturnStatement();
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(152);
				match(T__12);
				setState(153);
				((StmtContext)_localctx).expr = expr(0);
				setState(154);
				match(T__4);

				        ((StmtContext)_localctx).s =  new minicc::ReturnStatement();
				        ((minicc::ReturnStatement*)_localctx.s)->addChild(((StmtContext)_localctx).expr.e);
				        _localctx.s->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(157);
				((StmtContext)_localctx).scope = scope();

				        ((StmtContext)_localctx).s =  ((StmtContext)_localctx).scope.ss;
				    
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarlistContext extends ParserRuleContext {
		public std::vector<minicc::VarReference*> vLst;
		public VarlistContext v1;
		public VarlistentryContext varlistentry;
		public VarlistentryContext varlistentry() {
			return getRuleContext(VarlistentryContext.class,0);
		}
		public VarlistContext varlist() {
			return getRuleContext(VarlistContext.class,0);
		}
		public VarlistContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varlist; }
	}

	public final VarlistContext varlist() throws RecognitionException {
		return varlist(0);
	}

	private VarlistContext varlist(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		VarlistContext _localctx = new VarlistContext(_ctx, _parentState);
		VarlistContext _prevctx = _localctx;
		int _startState = 12;
		enterRecursionRule(_localctx, 12, RULE_varlist, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(163);
			((VarlistContext)_localctx).varlistentry = varlistentry();

			        _localctx.vLst.clear();
			        _localctx.vLst.push_back(((VarlistContext)_localctx).varlistentry.ve);
			    
			}
			_ctx.stop = _input.LT(-1);
			setState(173);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new VarlistContext(_parentctx, _parentState);
					_localctx.v1 = _prevctx;
					_localctx.v1 = _prevctx;
					pushNewRecursionContext(_localctx, _startState, RULE_varlist);
					setState(166);
					if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
					setState(167);
					match(T__13);
					setState(168);
					((VarlistContext)_localctx).varlistentry = varlistentry();

					                      ((VarlistContext)_localctx).vLst =  ((VarlistContext)_localctx).v1.vLst;
					                      _localctx.vLst.push_back(((VarlistContext)_localctx).varlistentry.ve);
					                  
					}
					} 
				}
				setState(175);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class VarlistentryContext extends ParserRuleContext {
		public minicc::VarReference * ve;
		public VarnameContext varname;
		public Token INT;
		public VarnameContext varname() {
			return getRuleContext(VarnameContext.class,0);
		}
		public TerminalNode INT() { return getToken(MiniCParser.INT, 0); }
		public VarlistentryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varlistentry; }
	}

	public final VarlistentryContext varlistentry() throws RecognitionException {
		VarlistentryContext _localctx = new VarlistentryContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_varlistentry);
		try {
			setState(185);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,7,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(176);
				((VarlistentryContext)_localctx).varname = varname();

				        ((VarlistentryContext)_localctx).ve =  new minicc::VarReference();
				        _localctx.ve->addChild(((VarlistentryContext)_localctx).varname.varName);
				        _localctx.ve->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(179);
				((VarlistentryContext)_localctx).varname = varname();
				setState(180);
				match(T__14);
				setState(181);
				((VarlistentryContext)_localctx).INT = match(INT);
				setState(182);
				match(T__15);

				            ((VarlistentryContext)_localctx).ve =  new minicc::VarReference();
				            minicc::ConstantLiteralExpr *n = minicc::ConstantLiteralExpr::fromString((((VarlistentryContext)_localctx).INT!=null?((VarlistentryContext)_localctx).INT.getText():null));
				            _localctx.ve->addChild(((VarlistentryContext)_localctx).varname.varName);
				            _localctx.ve->addChild(n);
				            n->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				            _localctx.ve->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VartypeContext extends ParserRuleContext {
		public minicc::TypeReference * t;
		public VartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vartype; }
	}

	public final VartypeContext vartype() throws RecognitionException {
		VartypeContext _localctx = new VartypeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_vartype);
		try {
			setState(191);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__16:
				enterOuterAlt(_localctx, 1);
				{
				setState(187);
				match(T__16);

				        ((VartypeContext)_localctx).t =  new minicc::TypeReference(Type::Int);
				        _localctx.t->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case T__17:
				enterOuterAlt(_localctx, 2);
				{
				setState(189);
				match(T__17);

				        ((VartypeContext)_localctx).t =  new minicc::TypeReference(Type::Bool);
				        _localctx.t->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RettypeContext extends ParserRuleContext {
		public minicc::TypeReference * rt;
		public VartypeContext vartype;
		public VartypeContext vartype() {
			return getRuleContext(VartypeContext.class,0);
		}
		public RettypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rettype; }
	}

	public final RettypeContext rettype() throws RecognitionException {
		RettypeContext _localctx = new RettypeContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_rettype);
		try {
			setState(198);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__18:
				enterOuterAlt(_localctx, 1);
				{
				setState(193);
				match(T__18);

					        ((RettypeContext)_localctx).rt =  new minicc::TypeReference(Type::Void);
				            _localctx.rt->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case T__16:
			case T__17:
				enterOuterAlt(_localctx, 2);
				{
				setState(195);
				((RettypeContext)_localctx).vartype = vartype();

				            ((RettypeContext)_localctx).rt =  ((RettypeContext)_localctx).vartype.t;
				            _localctx.rt->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParametersContext extends ParserRuleContext {
		public std::vector<minicc::Parameter*> params;
		public ParameterlistContext parameterlist;
		public ParameterlistContext parameterlist() {
			return getRuleContext(ParameterlistContext.class,0);
		}
		public ParametersContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parameters; }
	}

	public final ParametersContext parameters() throws RecognitionException {
		ParametersContext _localctx = new ParametersContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_parameters);
		try {
			setState(204);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__3:
				enterOuterAlt(_localctx, 1);
				{

				        _localctx.params.clear ();
				    
				}
				break;
			case T__16:
			case T__17:
				enterOuterAlt(_localctx, 2);
				{
				setState(201);
				((ParametersContext)_localctx).parameterlist = parameterlist(0);

				            ((ParametersContext)_localctx).params =  ((ParametersContext)_localctx).parameterlist.pLst;
				        
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParameterlistContext extends ParserRuleContext {
		public std::vector<minicc::Parameter*> pLst;
		public ParameterlistContext p1;
		public ParameterentryContext parameterentry;
		public ParameterentryContext parameterentry() {
			return getRuleContext(ParameterentryContext.class,0);
		}
		public ParameterlistContext parameterlist() {
			return getRuleContext(ParameterlistContext.class,0);
		}
		public ParameterlistContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parameterlist; }
	}

	public final ParameterlistContext parameterlist() throws RecognitionException {
		return parameterlist(0);
	}

	private ParameterlistContext parameterlist(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ParameterlistContext _localctx = new ParameterlistContext(_ctx, _parentState);
		ParameterlistContext _prevctx = _localctx;
		int _startState = 22;
		enterRecursionRule(_localctx, 22, RULE_parameterlist, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(207);
			((ParameterlistContext)_localctx).parameterentry = parameterentry();

				        _localctx.pLst.clear ();
			            _localctx.pLst.push_back(((ParameterlistContext)_localctx).parameterentry.p);
			        
			}
			_ctx.stop = _input.LT(-1);
			setState(217);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ParameterlistContext(_parentctx, _parentState);
					_localctx.p1 = _prevctx;
					_localctx.p1 = _prevctx;
					pushNewRecursionContext(_localctx, _startState, RULE_parameterlist);
					setState(210);
					if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
					setState(211);
					match(T__13);
					setState(212);
					((ParameterlistContext)_localctx).parameterentry = parameterentry();

					          	        ((ParameterlistContext)_localctx).pLst =  ((ParameterlistContext)_localctx).p1.pLst;
					                      _localctx.pLst.push_back(((ParameterlistContext)_localctx).parameterentry.p);
					                  
					}
					} 
				}
				setState(219);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,11,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class ParameterentryContext extends ParserRuleContext {
		public minicc::Parameter * p;
		public VartypeContext vartype;
		public ParameternameContext parametername;
		public VartypeContext vartype() {
			return getRuleContext(VartypeContext.class,0);
		}
		public ParameternameContext parametername() {
			return getRuleContext(ParameternameContext.class,0);
		}
		public ParameterentryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parameterentry; }
	}

	public final ParameterentryContext parameterentry() throws RecognitionException {
		ParameterentryContext _localctx = new ParameterentryContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_parameterentry);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(220);
			((ParameterentryContext)_localctx).vartype = vartype();
			setState(221);
			((ParameterentryContext)_localctx).parametername = parametername();

			        ((ParameterentryContext)_localctx).p =  new minicc::Parameter();
			        _localctx.p->addChild(((ParameterentryContext)_localctx).vartype.t);
			        _localctx.p->addChild(((ParameterentryContext)_localctx).parametername.param);
			        _localctx.p->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExproptContext extends ParserRuleContext {
		public minicc::Expr * eo;
		public ExprContext expr;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public ExproptContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expropt; }
	}

	public final ExproptContext expropt() throws RecognitionException {
		ExproptContext _localctx = new ExproptContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_expropt);
		try {
			setState(228);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__2:
			case T__19:
			case T__29:
			case T__33:
			case T__34:
			case ID:
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(224);
				((ExproptContext)_localctx).expr = expr(0);

				            ((ExproptContext)_localctx).eo =  ((ExproptContext)_localctx).expr.e;
				            _localctx.eo->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case T__3:
			case T__4:
				enterOuterAlt(_localctx, 2);
				{

				            ((ExproptContext)_localctx).eo =  nullptr;
				        
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public minicc::Expr * e;
		public bool compare;
		public ExprContext e1;
		public Token INT;
		public Token op;
		public ExprContext expr;
		public VarContext var;
		public Token v;
		public FuncnameContext funcname;
		public ArgumentsContext arguments;
		public ParameternameContext parametername;
		public ExprContext e2;
		public TerminalNode INT() { return getToken(MiniCParser.INT, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public VarContext var() {
			return getRuleContext(VarContext.class,0);
		}
		public FuncnameContext funcname() {
			return getRuleContext(FuncnameContext.class,0);
		}
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public ParameternameContext parametername() {
			return getRuleContext(ParameternameContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	}

	public final ExprContext expr() throws RecognitionException {
		return expr(0);
	}

	private ExprContext expr(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExprContext _localctx = new ExprContext(_ctx, _parentState);
		ExprContext _prevctx = _localctx;
		int _startState = 28;
		enterRecursionRule(_localctx, 28, RULE_expr, _p);
		 ((ExprContext)_localctx).compare = false;
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(267);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
			case 1:
				{
				setState(231);
				((ExprContext)_localctx).INT = match(INT);

				            ((ExprContext)_localctx).e =  minicc::ConstantLiteralExpr::fromString((((ExprContext)_localctx).INT!=null?((ExprContext)_localctx).INT.getText():null));
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 2:
				{
				setState(233);
				((ExprContext)_localctx).op = match(T__19);
				setState(234);
				((ExprContext)_localctx).e1 = ((ExprContext)_localctx).expr = expr(14);

				            if (typeid(*((ExprContext)_localctx).e1.e) == typeid(minicc::IntLiteralExpr)) {
				                delete ((ExprContext)_localctx).e1.e;
				                // check the int to see if it has an out of bound value. Note ConstantLiteralExpr will produce "void" type if the int is out of bound.
				                ((ExprContext)_localctx).e1.e = minicc::ConstantLiteralExpr::fromString((((ExprContext)_localctx).e1!=null?_input.getText(((ExprContext)_localctx).e1.start,((ExprContext)_localctx).e1.stop):null), true);
				                ((ExprContext)_localctx).e1.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine()); 
				            }
					        minicc::UnaryExpr *exp = new minicc::UnaryExpr();
					        exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
				            exp->addChild(((ExprContext)_localctx).e1.e);
				            ((ExprContext)_localctx).e =  exp;
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 3:
				{
				setState(237);
				((ExprContext)_localctx).op = match(T__29);
				setState(238);
				((ExprContext)_localctx).e1 = ((ExprContext)_localctx).expr = expr(10);

				            minicc::UnaryExpr *exp= new minicc::UnaryExpr();
				            exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
				            exp->addChild(((ExprContext)_localctx).e1.e);
					        ((ExprContext)_localctx).e =  exp;
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 4:
				{
				setState(241);
				((ExprContext)_localctx).var = var();
				setState(242);
				match(T__32);
				setState(243);
				((ExprContext)_localctx).expr = expr(7);

						    minicc::AssignmentExpr *exp = new minicc::AssignmentExpr();
				            exp->addChild(((ExprContext)_localctx).var.v);
				            exp->addChild(((ExprContext)_localctx).expr.e);
				            ((ExprContext)_localctx).e =  exp;
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 5:
				{
				setState(246);
				((ExprContext)_localctx).v = match(T__33);

				            ((ExprContext)_localctx).e =  new minicc::BoolLiteralExpr(true);
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 6:
				{
				setState(248);
				((ExprContext)_localctx).v = match(T__34);

				            ((ExprContext)_localctx).e =  new minicc::BoolLiteralExpr(false);
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 7:
				{
				setState(250);
				match(T__2);
				setState(251);
				((ExprContext)_localctx).expr = expr(0);
				setState(252);
				match(T__3);

				            ((ExprContext)_localctx).e =  ((ExprContext)_localctx).expr.e;
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 8:
				{
				setState(255);
				((ExprContext)_localctx).var = var();

				           ((ExprContext)_localctx).e =  new minicc::VarExpr(); // Assuming each VarExpr has a VarReference child
				           ((minicc::VarExpr*)_localctx.e)->addChild(((ExprContext)_localctx).var.v);
				           _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				        
				}
				break;
			case 9:
				{
				setState(258);
				((ExprContext)_localctx).funcname = funcname();
				setState(259);
				match(T__2);
				setState(260);
				((ExprContext)_localctx).arguments = arguments();
				setState(261);
				match(T__3);

				            ((ExprContext)_localctx).e =  new minicc::CallExpr();
				            ((minicc::CallExpr*)_localctx.e)->addChild(((ExprContext)_localctx).funcname.fName);
				            for(auto it = ((ExprContext)_localctx).arguments.args.begin() ; it != ((ExprContext)_localctx).arguments.args.end(); ++it) {
				                ((minicc::CallExpr*)_localctx.e)->addChild(*it);
				            }
				            _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 10:
				{
				setState(264);
				((ExprContext)_localctx).parametername = parametername();

					        ((ExprContext)_localctx).e =  new minicc::VarExpr();
				            minicc::VarReference *tmp = new minicc::VarReference();
				            tmp->addChild(((ExprContext)_localctx).parametername.param);
				            tmp->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				            ((minicc::VarExpr*)_localctx.e)->addChild(tmp);
				        
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(296);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					setState(294);
					_errHandler.sync(this);
					switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
					case 1:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.e1 = _prevctx;
						_localctx.e1 = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(269);
						if (!(precpred(_ctx, 13))) throw new FailedPredicateException(this, "precpred(_ctx, 13)");
						setState(270);
						((ExprContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !(_la==T__20 || _la==T__21) ) {
							((ExprContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(271);
						((ExprContext)_localctx).e2 = ((ExprContext)_localctx).expr = expr(14);

						                      minicc::BinaryExpr *exp = new minicc::BinaryExpr();
						                      exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
						                      exp->addChild(((ExprContext)_localctx).e1.e);
						                      exp->addChild(((ExprContext)_localctx).e2.e);
						                      ((ExprContext)_localctx).e =  exp;
						                      _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
						                  
						}
						break;
					case 2:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.e1 = _prevctx;
						_localctx.e1 = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(274);
						if (!(precpred(_ctx, 12))) throw new FailedPredicateException(this, "precpred(_ctx, 12)");
						setState(275);
						((ExprContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !(_la==T__19 || _la==T__22) ) {
							((ExprContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(276);
						((ExprContext)_localctx).e2 = ((ExprContext)_localctx).expr = expr(13);

						                      minicc::BinaryExpr *exp = new minicc::BinaryExpr();
						                      exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
						                      exp->addChild(((ExprContext)_localctx).e1.e);
						                      exp->addChild(((ExprContext)_localctx).e2.e);
						                      ((ExprContext)_localctx).e =  exp;
						                      _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
						                  
						}
						break;
					case 3:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.e1 = _prevctx;
						_localctx.e1 = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(279);
						if (!(precpred(_ctx, 11))) throw new FailedPredicateException(this, "precpred(_ctx, 11)");
						setState(280);
						((ExprContext)_localctx).op = _input.LT(1);
						_la = _input.LA(1);
						if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__23) | (1L << T__24) | (1L << T__25) | (1L << T__26) | (1L << T__27) | (1L << T__28))) != 0)) ) {
							((ExprContext)_localctx).op = (Token)_errHandler.recoverInline(this);
						}
						else {
							if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
							_errHandler.reportMatch(this);
							consume();
						}
						setState(281);
						((ExprContext)_localctx).e2 = ((ExprContext)_localctx).expr = expr(12);

						                      minicc::BinaryExpr *exp = new minicc::BinaryExpr();
						                      ((ExprContext)_localctx).compare =  true;
						                      exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
						                      exp->addChild(((ExprContext)_localctx).e1.e);
						                      exp->addChild(((ExprContext)_localctx).e2.e);
						          	        ((ExprContext)_localctx).e =  exp;
						                      _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
						                  
						}
						break;
					case 4:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.e1 = _prevctx;
						_localctx.e1 = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(284);
						if (!(precpred(_ctx, 9))) throw new FailedPredicateException(this, "precpred(_ctx, 9)");
						setState(285);
						((ExprContext)_localctx).op = match(T__30);
						setState(286);
						((ExprContext)_localctx).e2 = ((ExprContext)_localctx).expr = expr(10);

						                      minicc::BinaryExpr *exp = new minicc::BinaryExpr();
						                      exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
						                      exp->addChild(((ExprContext)_localctx).e1.e);
						                      exp->addChild(((ExprContext)_localctx).e2.e);
						          	        ((ExprContext)_localctx).e =  exp;
						                      _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
						                  
						}
						break;
					case 5:
						{
						_localctx = new ExprContext(_parentctx, _parentState);
						_localctx.e1 = _prevctx;
						_localctx.e1 = _prevctx;
						pushNewRecursionContext(_localctx, _startState, RULE_expr);
						setState(289);
						if (!(precpred(_ctx, 8))) throw new FailedPredicateException(this, "precpred(_ctx, 8)");
						setState(290);
						((ExprContext)_localctx).op = match(T__31);
						setState(291);
						((ExprContext)_localctx).e2 = ((ExprContext)_localctx).expr = expr(9);

						                      minicc::BinaryExpr *exp = new minicc::BinaryExpr();
						                      exp->setOpcode(minicc::Expr::opcodeFromString((((ExprContext)_localctx).op!=null?((ExprContext)_localctx).op.getText():null)));
						                      exp->addChild(((ExprContext)_localctx).e1.e);
						                      exp->addChild(((ExprContext)_localctx).e2.e);
						                      ((ExprContext)_localctx).e =  exp;
						                      _localctx.e->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
						                  
						}
						break;
					}
					} 
				}
				setState(298);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class VarContext extends ParserRuleContext {
		public minicc::VarReference * v;
		public VarnameContext varname;
		public ExprContext expr;
		public VarnameContext varname() {
			return getRuleContext(VarnameContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public VarContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_var; }
	}

	public final VarContext var() throws RecognitionException {
		VarContext _localctx = new VarContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_var);
		try {
			setState(308);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,16,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(299);
				((VarContext)_localctx).varname = varname();

				        ((VarContext)_localctx).v =  new minicc::VarReference();
				        _localctx.v->addChild(((VarContext)_localctx).varname.varName);
				        _localctx.v->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(302);
				((VarContext)_localctx).varname = varname();
				setState(303);
				match(T__14);
				setState(304);
				((VarContext)_localctx).expr = expr(0);
				setState(305);
				match(T__15);

				        ((VarContext)_localctx).v =  new minicc::VarReference();
				        _localctx.v->addChild(((VarContext)_localctx).varname.varName);
				        _localctx.v->addChild(((VarContext)_localctx).expr.e);
				        _localctx.v->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
				    
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgumentsContext extends ParserRuleContext {
		public std::vector<minicc::Expr*> args;
		public ArgumentlistContext argumentlist;
		public ArgumentlistContext argumentlist() {
			return getRuleContext(ArgumentlistContext.class,0);
		}
		public ArgumentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arguments; }
	}

	public final ArgumentsContext arguments() throws RecognitionException {
		ArgumentsContext _localctx = new ArgumentsContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_arguments);
		try {
			setState(314);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__3:
				enterOuterAlt(_localctx, 1);
				{

				        _localctx.args.clear();
				    
				}
				break;
			case T__2:
			case T__19:
			case T__29:
			case T__33:
			case T__34:
			case ID:
			case INT:
				enterOuterAlt(_localctx, 2);
				{
				setState(311);
				((ArgumentsContext)_localctx).argumentlist = argumentlist(0);

				            ((ArgumentsContext)_localctx).args =  ((ArgumentsContext)_localctx).argumentlist.argsLst;
				    
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgumentlistContext extends ParserRuleContext {
		public std::vector<minicc::Expr*> argsLst;
		public ArgumentlistContext a1;
		public ExprContext expr;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public ArgumentlistContext argumentlist() {
			return getRuleContext(ArgumentlistContext.class,0);
		}
		public ArgumentlistContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argumentlist; }
	}

	public final ArgumentlistContext argumentlist() throws RecognitionException {
		return argumentlist(0);
	}

	private ArgumentlistContext argumentlist(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ArgumentlistContext _localctx = new ArgumentlistContext(_ctx, _parentState);
		ArgumentlistContext _prevctx = _localctx;
		int _startState = 34;
		enterRecursionRule(_localctx, 34, RULE_argumentlist, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(317);
			((ArgumentlistContext)_localctx).expr = expr(0);

			        _localctx.argsLst.clear();
			        _localctx.argsLst.push_back(((ArgumentlistContext)_localctx).expr.e);
			    
			}
			_ctx.stop = _input.LT(-1);
			setState(327);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ArgumentlistContext(_parentctx, _parentState);
					_localctx.a1 = _prevctx;
					_localctx.a1 = _prevctx;
					pushNewRecursionContext(_localctx, _startState, RULE_argumentlist);
					setState(320);
					if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
					setState(321);
					match(T__13);
					setState(322);
					((ArgumentlistContext)_localctx).expr = expr(0);

					          	        ((ArgumentlistContext)_localctx).argsLst =  ((ArgumentlistContext)_localctx).a1.argsLst;
					                      _localctx.argsLst.push_back(((ArgumentlistContext)_localctx).expr.e);
					              
					}
					} 
				}
				setState(329);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class VarnameContext extends ParserRuleContext {
		public minicc::Identifier * varName;
		public Token ID;
		public TerminalNode ID() { return getToken(MiniCParser.ID, 0); }
		public VarnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varname; }
	}

	public final VarnameContext varname() throws RecognitionException {
		VarnameContext _localctx = new VarnameContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_varname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(330);
			((VarnameContext)_localctx).ID = match(ID);

			        ((VarnameContext)_localctx).varName =  new minicc::Identifier((((VarnameContext)_localctx).ID!=null?((VarnameContext)_localctx).ID.getText():null));
			        _localctx.varName->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FuncnameContext extends ParserRuleContext {
		public minicc::Identifier * fName;
		public Token ID;
		public TerminalNode ID() { return getToken(MiniCParser.ID, 0); }
		public FuncnameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcname; }
	}

	public final FuncnameContext funcname() throws RecognitionException {
		FuncnameContext _localctx = new FuncnameContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_funcname);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(333);
			((FuncnameContext)_localctx).ID = match(ID);

			        ((FuncnameContext)_localctx).fName =  new minicc::Identifier((((FuncnameContext)_localctx).ID!=null?((FuncnameContext)_localctx).ID.getText():null));
			        _localctx.fName->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParameternameContext extends ParserRuleContext {
		public minicc::Identifier * param;
		public Token ID;
		public TerminalNode ID() { return getToken(MiniCParser.ID, 0); }
		public ParameternameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parametername; }
	}

	public final ParameternameContext parametername() throws RecognitionException {
		ParameternameContext _localctx = new ParameternameContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_parametername);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(336);
			((ParameternameContext)_localctx).ID = match(ID);

			        ((ParameternameContext)_localctx).param =  new minicc::Identifier((((ParameternameContext)_localctx).ID!=null?((ParameternameContext)_localctx).ID.getText():null));
				    _localctx.param->setSrcLoc(_localctx->start->getLine(), _localctx->start->getCharPositionInLine());
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 6:
			return varlist_sempred((VarlistContext)_localctx, predIndex);
		case 11:
			return parameterlist_sempred((ParameterlistContext)_localctx, predIndex);
		case 14:
			return expr_sempred((ExprContext)_localctx, predIndex);
		case 17:
			return argumentlist_sempred((ArgumentlistContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean varlist_sempred(VarlistContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 1);
		}
		return true;
	}
	private boolean parameterlist_sempred(ParameterlistContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1:
			return precpred(_ctx, 1);
		}
		return true;
	}
	private boolean expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 2:
			return precpred(_ctx, 13);
		case 3:
			return precpred(_ctx, 12);
		case 4:
			return precpred(_ctx, 11);
		case 5:
			return precpred(_ctx, 9);
		case 6:
			return precpred(_ctx, 8);
		}
		return true;
	}
	private boolean argumentlist_sempred(ArgumentlistContext _localctx, int predIndex) {
		switch (predIndex) {
		case 7:
			return precpred(_ctx, 1);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3)\u0156\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\3\2\3\2\3\2\3\2\5\2\61\n\2\3"+
		"\2\3\2\3\2\7\2\66\n\2\f\2\16\29\13\2\3\2\3\2\3\2\3\3\3\3\3\3\3\4\3\4\3"+
		"\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\5\4"+
		"T\n\4\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\7\6_\n\6\f\6\16\6b\13\6\3\6"+
		"\3\6\3\6\7\6g\n\6\f\6\16\6j\13\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3"+
		"\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7"+
		"\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3"+
		"\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\5\7\u00a3\n\7\3\b\3\b\3\b\3"+
		"\b\3\b\3\b\3\b\3\b\3\b\7\b\u00ae\n\b\f\b\16\b\u00b1\13\b\3\t\3\t\3\t\3"+
		"\t\3\t\3\t\3\t\3\t\3\t\5\t\u00bc\n\t\3\n\3\n\3\n\3\n\5\n\u00c2\n\n\3\13"+
		"\3\13\3\13\3\13\3\13\5\13\u00c9\n\13\3\f\3\f\3\f\3\f\5\f\u00cf\n\f\3\r"+
		"\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\7\r\u00da\n\r\f\r\16\r\u00dd\13\r\3\16"+
		"\3\16\3\16\3\16\3\17\3\17\3\17\3\17\5\17\u00e7\n\17\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\5\20\u010e\n\20\3\20\3\20\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\7\20\u0129\n\20\f\20\16\20\u012c\13\20\3\21"+
		"\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\5\21\u0137\n\21\3\22\3\22\3\22"+
		"\3\22\5\22\u013d\n\22\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\7\23"+
		"\u0148\n\23\f\23\16\23\u014b\13\23\3\24\3\24\3\24\3\25\3\25\3\25\3\26"+
		"\3\26\3\26\3\26\2\6\16\30\36$\27\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36"+
		" \"$&(*\2\5\3\2\27\30\4\2\26\26\31\31\3\2\32\37\2\u0166\2\60\3\2\2\2\4"+
		"=\3\2\2\2\6S\3\2\2\2\bU\3\2\2\2\nZ\3\2\2\2\f\u00a2\3\2\2\2\16\u00a4\3"+
		"\2\2\2\20\u00bb\3\2\2\2\22\u00c1\3\2\2\2\24\u00c8\3\2\2\2\26\u00ce\3\2"+
		"\2\2\30\u00d0\3\2\2\2\32\u00de\3\2\2\2\34\u00e6\3\2\2\2\36\u010d\3\2\2"+
		"\2 \u0136\3\2\2\2\"\u013c\3\2\2\2$\u013e\3\2\2\2&\u014c\3\2\2\2(\u014f"+
		"\3\2\2\2*\u0152\3\2\2\2,-\5\4\3\2-.\b\2\1\2.\61\3\2\2\2/\61\3\2\2\2\60"+
		",\3\2\2\2\60/\3\2\2\2\61\67\3\2\2\2\62\63\5\6\4\2\63\64\b\2\1\2\64\66"+
		"\3\2\2\2\65\62\3\2\2\2\669\3\2\2\2\67\65\3\2\2\2\678\3\2\2\28:\3\2\2\2"+
		"9\67\3\2\2\2:;\7\2\2\3;<\b\2\1\2<\3\3\2\2\2=>\7\3\2\2>?\7\4\2\2?\5\3\2"+
		"\2\2@A\5\b\5\2AB\b\4\1\2BT\3\2\2\2CD\5\24\13\2DE\5(\25\2EF\7\5\2\2FG\5"+
		"\26\f\2GH\7\6\2\2HI\5\n\6\2IJ\b\4\1\2JT\3\2\2\2KL\5\24\13\2LM\5(\25\2"+
		"MN\7\5\2\2NO\5\26\f\2OP\7\6\2\2PQ\7\7\2\2QR\b\4\1\2RT\3\2\2\2S@\3\2\2"+
		"\2SC\3\2\2\2SK\3\2\2\2T\7\3\2\2\2UV\5\22\n\2VW\5\16\b\2WX\7\7\2\2XY\b"+
		"\5\1\2Y\t\3\2\2\2Z`\7\b\2\2[\\\5\b\5\2\\]\b\6\1\2]_\3\2\2\2^[\3\2\2\2"+
		"_b\3\2\2\2`^\3\2\2\2`a\3\2\2\2ah\3\2\2\2b`\3\2\2\2cd\5\f\7\2de\b\6\1\2"+
		"eg\3\2\2\2fc\3\2\2\2gj\3\2\2\2hf\3\2\2\2hi\3\2\2\2ik\3\2\2\2jh\3\2\2\2"+
		"kl\7\t\2\2lm\b\6\1\2m\13\3\2\2\2no\5\36\20\2op\7\7\2\2pq\b\7\1\2q\u00a3"+
		"\3\2\2\2rs\7\n\2\2st\7\5\2\2tu\5\36\20\2uv\7\6\2\2vw\5\f\7\2wx\b\7\1\2"+
		"x\u00a3\3\2\2\2yz\7\n\2\2z{\7\5\2\2{|\5\36\20\2|}\7\6\2\2}~\5\f\7\2~\177"+
		"\7\13\2\2\177\u0080\5\f\7\2\u0080\u0081\b\7\1\2\u0081\u00a3\3\2\2\2\u0082"+
		"\u0083\7\f\2\2\u0083\u0084\7\5\2\2\u0084\u0085\5\34\17\2\u0085\u0086\7"+
		"\7\2\2\u0086\u0087\5\34\17\2\u0087\u0088\7\7\2\2\u0088\u0089\5\34\17\2"+
		"\u0089\u008a\7\6\2\2\u008a\u008b\5\f\7\2\u008b\u008c\b\7\1\2\u008c\u00a3"+
		"\3\2\2\2\u008d\u008e\7\r\2\2\u008e\u008f\7\5\2\2\u008f\u0090\5\34\17\2"+
		"\u0090\u0091\7\6\2\2\u0091\u0092\5\f\7\2\u0092\u0093\b\7\1\2\u0093\u00a3"+
		"\3\2\2\2\u0094\u0095\7\16\2\2\u0095\u0096\7\7\2\2\u0096\u00a3\b\7\1\2"+
		"\u0097\u0098\7\17\2\2\u0098\u0099\7\7\2\2\u0099\u00a3\b\7\1\2\u009a\u009b"+
		"\7\17\2\2\u009b\u009c\5\36\20\2\u009c\u009d\7\7\2\2\u009d\u009e\b\7\1"+
		"\2\u009e\u00a3\3\2\2\2\u009f\u00a0\5\n\6\2\u00a0\u00a1\b\7\1\2\u00a1\u00a3"+
		"\3\2\2\2\u00a2n\3\2\2\2\u00a2r\3\2\2\2\u00a2y\3\2\2\2\u00a2\u0082\3\2"+
		"\2\2\u00a2\u008d\3\2\2\2\u00a2\u0094\3\2\2\2\u00a2\u0097\3\2\2\2\u00a2"+
		"\u009a\3\2\2\2\u00a2\u009f\3\2\2\2\u00a3\r\3\2\2\2\u00a4\u00a5\b\b\1\2"+
		"\u00a5\u00a6\5\20\t\2\u00a6\u00a7\b\b\1\2\u00a7\u00af\3\2\2\2\u00a8\u00a9"+
		"\f\3\2\2\u00a9\u00aa\7\20\2\2\u00aa\u00ab\5\20\t\2\u00ab\u00ac\b\b\1\2"+
		"\u00ac\u00ae\3\2\2\2\u00ad\u00a8\3\2\2\2\u00ae\u00b1\3\2\2\2\u00af\u00ad"+
		"\3\2\2\2\u00af\u00b0\3\2\2\2\u00b0\17\3\2\2\2\u00b1\u00af\3\2\2\2\u00b2"+
		"\u00b3\5&\24\2\u00b3\u00b4\b\t\1\2\u00b4\u00bc\3\2\2\2\u00b5\u00b6\5&"+
		"\24\2\u00b6\u00b7\7\21\2\2\u00b7\u00b8\7\'\2\2\u00b8\u00b9\7\22\2\2\u00b9"+
		"\u00ba\b\t\1\2\u00ba\u00bc\3\2\2\2\u00bb\u00b2\3\2\2\2\u00bb\u00b5\3\2"+
		"\2\2\u00bc\21\3\2\2\2\u00bd\u00be\7\23\2\2\u00be\u00c2\b\n\1\2\u00bf\u00c0"+
		"\7\24\2\2\u00c0\u00c2\b\n\1\2\u00c1\u00bd\3\2\2\2\u00c1\u00bf\3\2\2\2"+
		"\u00c2\23\3\2\2\2\u00c3\u00c4\7\25\2\2\u00c4\u00c9\b\13\1\2\u00c5\u00c6"+
		"\5\22\n\2\u00c6\u00c7\b\13\1\2\u00c7\u00c9\3\2\2\2\u00c8\u00c3\3\2\2\2"+
		"\u00c8\u00c5\3\2\2\2\u00c9\25\3\2\2\2\u00ca\u00cf\b\f\1\2\u00cb\u00cc"+
		"\5\30\r\2\u00cc\u00cd\b\f\1\2\u00cd\u00cf\3\2\2\2\u00ce\u00ca\3\2\2\2"+
		"\u00ce\u00cb\3\2\2\2\u00cf\27\3\2\2\2\u00d0\u00d1\b\r\1\2\u00d1\u00d2"+
		"\5\32\16\2\u00d2\u00d3\b\r\1\2\u00d3\u00db\3\2\2\2\u00d4\u00d5\f\3\2\2"+
		"\u00d5\u00d6\7\20\2\2\u00d6\u00d7\5\32\16\2\u00d7\u00d8\b\r\1\2\u00d8"+
		"\u00da\3\2\2\2\u00d9\u00d4\3\2\2\2\u00da\u00dd\3\2\2\2\u00db\u00d9\3\2"+
		"\2\2\u00db\u00dc\3\2\2\2\u00dc\31\3\2\2\2\u00dd\u00db\3\2\2\2\u00de\u00df"+
		"\5\22\n\2\u00df\u00e0\5*\26\2\u00e0\u00e1\b\16\1\2\u00e1\33\3\2\2\2\u00e2"+
		"\u00e3\5\36\20\2\u00e3\u00e4\b\17\1\2\u00e4\u00e7\3\2\2\2\u00e5\u00e7"+
		"\b\17\1\2\u00e6\u00e2\3\2\2\2\u00e6\u00e5\3\2\2\2\u00e7\35\3\2\2\2\u00e8"+
		"\u00e9\b\20\1\2\u00e9\u00ea\7\'\2\2\u00ea\u010e\b\20\1\2\u00eb\u00ec\7"+
		"\26\2\2\u00ec\u00ed\5\36\20\20\u00ed\u00ee\b\20\1\2\u00ee\u010e\3\2\2"+
		"\2\u00ef\u00f0\7 \2\2\u00f0\u00f1\5\36\20\f\u00f1\u00f2\b\20\1\2\u00f2"+
		"\u010e\3\2\2\2\u00f3\u00f4\5 \21\2\u00f4\u00f5\7#\2\2\u00f5\u00f6\5\36"+
		"\20\t\u00f6\u00f7\b\20\1\2\u00f7\u010e\3\2\2\2\u00f8\u00f9\7$\2\2\u00f9"+
		"\u010e\b\20\1\2\u00fa\u00fb\7%\2\2\u00fb\u010e\b\20\1\2\u00fc\u00fd\7"+
		"\5\2\2\u00fd\u00fe\5\36\20\2\u00fe\u00ff\7\6\2\2\u00ff\u0100\b\20\1\2"+
		"\u0100\u010e\3\2\2\2\u0101\u0102\5 \21\2\u0102\u0103\b\20\1\2\u0103\u010e"+
		"\3\2\2\2\u0104\u0105\5(\25\2\u0105\u0106\7\5\2\2\u0106\u0107\5\"\22\2"+
		"\u0107\u0108\7\6\2\2\u0108\u0109\b\20\1\2\u0109\u010e\3\2\2\2\u010a\u010b"+
		"\5*\26\2\u010b\u010c\b\20\1\2\u010c\u010e\3\2\2\2\u010d\u00e8\3\2\2\2"+
		"\u010d\u00eb\3\2\2\2\u010d\u00ef\3\2\2\2\u010d\u00f3\3\2\2\2\u010d\u00f8"+
		"\3\2\2\2\u010d\u00fa\3\2\2\2\u010d\u00fc\3\2\2\2\u010d\u0101\3\2\2\2\u010d"+
		"\u0104\3\2\2\2\u010d\u010a\3\2\2\2\u010e\u012a\3\2\2\2\u010f\u0110\f\17"+
		"\2\2\u0110\u0111\t\2\2\2\u0111\u0112\5\36\20\20\u0112\u0113\b\20\1\2\u0113"+
		"\u0129\3\2\2\2\u0114\u0115\f\16\2\2\u0115\u0116\t\3\2\2\u0116\u0117\5"+
		"\36\20\17\u0117\u0118\b\20\1\2\u0118\u0129\3\2\2\2\u0119\u011a\f\r\2\2"+
		"\u011a\u011b\t\4\2\2\u011b\u011c\5\36\20\16\u011c\u011d\b\20\1\2\u011d"+
		"\u0129\3\2\2\2\u011e\u011f\f\13\2\2\u011f\u0120\7!\2\2\u0120\u0121\5\36"+
		"\20\f\u0121\u0122\b\20\1\2\u0122\u0129\3\2\2\2\u0123\u0124\f\n\2\2\u0124"+
		"\u0125\7\"\2\2\u0125\u0126\5\36\20\13\u0126\u0127\b\20\1\2\u0127\u0129"+
		"\3\2\2\2\u0128\u010f\3\2\2\2\u0128\u0114\3\2\2\2\u0128\u0119\3\2\2\2\u0128"+
		"\u011e\3\2\2\2\u0128\u0123\3\2\2\2\u0129\u012c\3\2\2\2\u012a\u0128\3\2"+
		"\2\2\u012a\u012b\3\2\2\2\u012b\37\3\2\2\2\u012c\u012a\3\2\2\2\u012d\u012e"+
		"\5&\24\2\u012e\u012f\b\21\1\2\u012f\u0137\3\2\2\2\u0130\u0131\5&\24\2"+
		"\u0131\u0132\7\21\2\2\u0132\u0133\5\36\20\2\u0133\u0134\7\22\2\2\u0134"+
		"\u0135\b\21\1\2\u0135\u0137\3\2\2\2\u0136\u012d\3\2\2\2\u0136\u0130\3"+
		"\2\2\2\u0137!\3\2\2\2\u0138\u013d\b\22\1\2\u0139\u013a\5$\23\2\u013a\u013b"+
		"\b\22\1\2\u013b\u013d\3\2\2\2\u013c\u0138\3\2\2\2\u013c\u0139\3\2\2\2"+
		"\u013d#\3\2\2\2\u013e\u013f\b\23\1\2\u013f\u0140\5\36\20\2\u0140\u0141"+
		"\b\23\1\2\u0141\u0149\3\2\2\2\u0142\u0143\f\3\2\2\u0143\u0144\7\20\2\2"+
		"\u0144\u0145\5\36\20\2\u0145\u0146\b\23\1\2\u0146\u0148\3\2\2\2\u0147"+
		"\u0142\3\2\2\2\u0148\u014b\3\2\2\2\u0149\u0147\3\2\2\2\u0149\u014a\3\2"+
		"\2\2\u014a%\3\2\2\2\u014b\u0149\3\2\2\2\u014c\u014d\7&\2\2\u014d\u014e"+
		"\b\24\1\2\u014e\'\3\2\2\2\u014f\u0150\7&\2\2\u0150\u0151\b\25\1\2\u0151"+
		")\3\2\2\2\u0152\u0153\7&\2\2\u0153\u0154\b\26\1\2\u0154+\3\2\2\2\25\60"+
		"\67S`h\u00a2\u00af\u00bb\u00c1\u00c8\u00ce\u00db\u00e6\u010d\u0128\u012a"+
		"\u0136\u013c\u0149";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}