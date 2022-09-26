grammar AlgParser;

@lexer::members {
    //boolean interpolated = false;
    int nestLevel = 0;
    //int iLevel = 0;
    java.util.LinkedList<Integer> iLevels = new java.util.LinkedList<Integer>();
}

/*@header {
    package io.opsit.explang.algparser;
}*/
// TODO:
// exponential notation >> unary - + operators
// power (higher precision)

expr    :   beblock                                         # beblock_expr
    |  IF expr block (ELSEIF expr block)*  ( ELSE block )? EB #if_expr
    |  WHILE expr block EB                                  # while_expr
    |  FOR  expr INOP expr (RESULT expr)? block EB          # for_expr
    |  RETURN expr                                          # return_expr
    |       expr ( '.' SYMBOL )+                            # dotchain
    |       expr ( vector )+                                # assoc_lookup
    |       lambda                                          # lambda_expr
    |       fsymbol '('  exprList?  ')'                     # funcall_expr
    |       '(' lambda ')' '('  exprList?  ')'              # lambdacall_expr        
    |       LP expr RP                                      # paren_expr
    |       op=( SUBOP | ADDOP ) expr                       # sign_expr
    |       NOTOP                expr                       # not_expr
    |       expr  op=( MULOP | DIVOP | REMOP ) expr         # product_expr
    |       expr  op=( ADDOP | SUBOP ) expr                 # sum_expr
    |       expr  op=( NUMLT | NUMGT | NUMGE | NUMLE ) expr # numcomp_expr
    |       expr  op=( ISSAME | EQUAL | NOTEQUAL | NUMEQUAL) expr  # equality_expr
    |       expr  ANDOP    expr                             # and_expr
    |       expr  OROP     expr                             # or_expr
    |       expr  INOP     expr                             # in_expr
    |       expr  DWIM_MATCHES expr                         # dwim_matches_expr
    |       expr  SEARCHOP  expr                            # dwim_search_expr        
    |       expr  FIELDSOP  SYMBOL (',' SYMBOL)*            # fields_expr
//    |<assoc=right>       expr  ( ASOP SYMBOL)?   '|' rs=expr  { !$rs.text.startsWith("[")  }?         # th_as_expr
    |<assoc=right>       expr   '|' ((vector)+ | expr )     # th_auto_expr        
    |<assoc=right>       expr  ASOP SYMBOL   '|'  expr      # th_as_expr

    |       expr '|'                                        # th_at_expr
    |       SYMBOL GASSIGN  expr                            # gassign_expr
    |       expr   LASSIGN  expr                            # assign_expr
//    |       SYMFUNC                                         # symfunc_expr
    |       (IPOL_START expr (IPOL_MIDDLE expr )* IPOL_END | IPOL_VOID) # ipol_expr
    |       ('{' '}' | '{' expr ':' expr (',' expr ':' expr )* '}')     # dict_expr
    |       vector                                          # vector_expr
    |       ':' expr                                        # quote_expr
    |       CHAR                                            # char_expr
    |       atom                                            # atom_expr
    |       '('  exprList?  ')' '->' expr                   # slambda_expr
    |       SYMBOL '->' expr                                # monolambda_expr                
    ; /* end of expr */

lambda   : FUNC SYMBOL? '(' (posargs=exprList rest=ELLIPSIS? )? ( ';' kwargs=exprList okeys=ELLIPSIS? )? ')' block EB ;
beblock  :  BB block EB                            ;
replblock : block EOF                              ;

vector   : '['  exprList?  ']' ;
exprList : expr (',' expr)*       ;
block    : expr (';' expr)*  ';'? ;
fsymbol  : ( SYMBOL
        | ADDOP | SUBOP | ANDOP | OROP | MULOP | DIVOP
        | NUMLT | NUMGT | NUMGE | NUMLE
        | NUMEQUAL | EQUAL | NOTEQUAL | ISSAME | INOP | DWIM_MATCHES ) ;
atom     : ( NIL_LIT | NUMBER | TRUE_LIT | FALSE_LIT | fsymbol  | STRING | REGEXP | SYMFUNC | VERSION );

//funop    : ( MULOP | DIVOP | ADDOP | SUBOP | ANDOP | 
/* lexer rules */

REGEXP   : [rg]'"'ESCSTR'"'[dixmlsucU]*;
SYMFUNC  : [f]'"'ESCSTR'"';
VERSION  : [v]'"'ESCSTR'"';

CHAR     : '\'' ('\\'. | '\\'[uU] HD HD? HD? HD? |~('\'') ) '\'' ;

STRING   : '"'ESCSTR'"' ;
WS       : [ \t\r\n]+ -> channel(HIDDEN);
/* FIXME: allow UNICODE letters */

FOR      : [fF][oO][rR];
RETURN   : [rR][eE][tT][uU][rR][nN];
RESULT   : [rR][eE][sS][uU][lL][tT];
IF       : [iI][fF];
ELSE     : [eE][lL][sS][eE];
ELSEIF     : [eE][lL][sS][eE][iI][fF];
WHILE    : [wW][hH][iI][lL][eE];
SEARCHOP : [sS][eE][aA][rR][cC][hH];
ASOP     : [aA][sS];
FIELDSOP : [fF][iI][eE][lL][dD][sS];
FUNC     : [fF][uU][nN][cC][tT][iI][oO][nN] ;
BB       : [bB][eE][gG][iI][nN];
EB       : [eE][nN][dD];

ANDOP    : [aA][nN][dD];
OROP     : [oO][rR];
INOP     : [iI][nN]; 
NIL_LIT  : ([nN][uU][lL][lL]|[nN][iI][lL]);
TRUE_LIT : [tT][rR][uU][eE];
FALSE_LIT: [fF][aA][lL][sS][eE];
NOTOP    : [nN][oO][tT];


SYMBOL   : [A-Za-z_][A-Za-z_0-9!]*;
//KEYWORD  : [:][A-Za-z_0-9]+;
NUMBER   : DIGIT+(DOT DIGIT+)?([eE][-]?DIGIT+)?[fFlLbBsSdDiI]? ; 
DIGIT    : [0-9];
HD       : [0-9A-Fa-f];
DOT      : '.' ;   

ISSAME   : '==='; 
EQUAL    : '==';
NOTEQUAL : '!=';
NUMEQUAL : '=';
NUMLT    : '<';  
NUMGT    : '>';  
NUMGE    : '>='; 
NUMLE    : '<='; 
ADDOP    : '+';  
SUBOP    : '-';  
DIVOP    : '/';  
MULOP    : '*';  
REMOP    : '%';  

LASSIGN  : ':=';
GASSIGN  : '::=';

DWIM_MATCHES : '=~' ;
//DWIM_SEARCH : [sS][eE][aA][rR][cC][hH] ;
ELLIPSIS : '...';
    
LP : '('  { nestLevel++; } ;
RP : ')'  { nestLevel--; } ;

IPOL_VOID  : [i]'"'IPOLSTR'"' ;
IPOL_START  : [i]'"'IPOLSTR'$('  { iLevels.push(nestLevel);}; 
IPOL_END  : ')'IPOLSTR'"'        { (!iLevels.isEmpty()) && ((int)iLevels.getFirst()) == nestLevel }? { iLevels.pop();};
IPOL_MIDDLE  : ')'IPOLSTR'$('    { (!iLevels.isEmpty()) && ((int)iLevels.getFirst()) == nestLevel }?;

fragment IPOLSTR: ('\\'[$btnr"\\]|~["$])*? ;
fragment ESCSTR: (ESC|.)*?;

fragment ESC:'\\'[btnr"\\]     ;

BLOCK_COMMENT: '#=' .*? '=#' -> channel(HIDDEN);
LINE_COMMENT: '#'~[=].*?'\n' ->channel(HIDDEN);


