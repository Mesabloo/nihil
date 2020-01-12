{-| The concrete syntax is a 1:1 representation of what the user
    wrote. It can be printed back as is (or at least most of it) to the user.
-}

module Nihil.Syntax.Concrete
( -- * Grammar used
  -- $grammar

  -- * Complete grammar of the language
  -- $bnf

  -- * Re-exports
  module Nihil.Syntax.Concrete.Core
, lProgram
, pProgram
) where

import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Lexer.Program (lProgram)
import Nihil.Syntax.Concrete.Parser.Statement (pProgram)

{- $grammar
    The grammar used below can be summarized with these rules:

    - @*@ means “0 or more times”. It is analogous to megaparsec's @many@.

    - @+@ means “at least once”. It is analogous to megaparsec's @some@.

    - @?@ means “optional”. It is analogous to megaparsec's @optional@.

    - @|@ means “or else”. It implies megaparsec's @try@ for the first case, and is analogous to megaparsec's @(\<|\>)@.

    - Literal strings are matched "as-is". It is analogous to megaparsec's @string@.

    - @::=@ means “is defined by”.

    This grammar however does not take into account indentation. If you'd like to implement it
    for a parser, it's up to you to manage indentation.

    This grammar also doesn't take into account operator precedence nor infix expression parsing.
-}

{- $bnf
    == Tokens:

    > Character           ::= ('\\' Escape-Character) | Unicode ;
    > Lower-Character     ::= Unicode-Lower ;
    > Upper-Character     ::= Unicode-Upper ;
    > Escape-Character    ::= 'n' | 'r' | 'e' | 'a' | 't' ;
    > Decimal-Digit       ::= '0'..'9' ;
    > Hexadecimal-Digit   ::= '0'..'9' | 'a'..'f' | 'A'..'F' ;
    > Binary-Digit        ::= '0' | '1' ;
    > Octal-Digit         ::= '0'..'7' ;
    >
    > STRING-LITERAL      ::= '"' Character* '"' ;
    > CHARACTER-LITERAL   ::= '\'' Character '\'' ;
    > INTEGER-LITERAL     ::= HEXADECIMAL-LITERAL | OCTAL-LITERAL | BINARY-LITERAL | DECIMAL-LITERAL ;
    > HEXADECIMAL-LITERAL ::= '0' ('x' | 'X') Hexadecimal-Digit+ ;
    > OCTAL-LITERAL       ::= '0' ('o' | 'O') Octal-Digit+ ;
    > BINARY-LITERAL      ::= '0' ('b' | 'B') Binary-Digit+ ;
    > DECIMAL-LITERAL     ::= Decimal-Digit+ ;
    > FLOAT-LITERAL       ::= DECIMAL-LITERAL+ '.' DECIMAL-LITERAL+ (('e' | 'E') DECIMAL-LITERAL+)? ;
    > LINE-COMMENT        ::= '--' (Unicode excluding '\n')* ('\n' | EOF) ;
    > BLOCK-COMMENT       ::= '{' '-' Unicode* '-' '}' ;  {- this one is written like that because of nested block comments -}
    > IDENTIFIER          ::= Lower-Character (Unicode-NoSymbol | '\'' | '_')* excluding KEYWORDS ;
    > TYPE-IDENTIFIER     ::= Upper-Character (Unicode-NoSymbol | '\'' | '_')* ;
    > OPERATOR            ::= (Unicode-Symbol+ | '`' (IDENTIFIER | TYPE-IDENTIFIER) '`') excluding OPERATORS ;
    > UNDERSCORE          ::= '_'+ ;

    == Reserved:

    > KEYWORDS            ::= 'match' | 'with' | 'data' | 'type' | 'let' | 'in' | 'where' | 'infixl' | 'infixr' ;
    > OPERATORS           ::= '=' | ':' | '\\' | '->' | '=>' | ',' | '→' | '⇒' | '`' | 'λ' ;

    == Parsing rules:

    === Expressions

    > expression          ::= (OPERATOR | eApp)+ (':' type | 'where' funStatements)? ;
    > eApp                ::= eAtoms eAtoms+? ;
    > eAtoms              ::= eAtom+ ;
    > eAtom               ::= eTypeHole
    >                       | eLambda
    >                       | eMatch
    >                       | eTuple
    >                       | eLetIn
    >                       | IDENTIFIER | '(' OPERATOR ')' | TYPE-IDENTIFIER
    >                       | FLOAT-LITERAL | INTEGER-LITERAL | CHARACTER-LITERAL | STRING-LITERAL
    >                       | '(' expression ')' ;
    > eTypeHole           ::= UNDERSCORE ;
    > eLambda             ::= ('\\' | 'λ') pattern+ ('->' | '→') expression ;
    > eMatch              ::= 'match' expression 'with' eMatchBranch+ ;
    > eMatchBranch        ::= pattern ('->' | '→') expression ;
    > eTuple              ::= '(' expression (',' expression)+ ')' ;
    > eLetIn              ::= 'let' funStatements+ 'in' expression ;

    === Patterns

    > pattern             ::= (OPERATOR | pAtom)+ ;
    > pAtom               ::= pAtom' (':' type)? ;
    > pAtom'              ::= pWildcard
    >                       | pConstructor
    >                       | pTuple
    >                       | IDENTIFIER
    >                       | FLOAT-LITERAL | INTEGER-LITERAL | CHARACTER-LITERAL | STRING-LITERAL ;
    > pWildcard           ::= UNDERSCORE ;
    > pConstructor        ::= TYPE-IDENTIFIER pAtom'* ;
    > pTuple              ::= '(' pattern (',' pattern)+ ')' ;

    === Types

    > type                ::= (OPERATOR | tAtom)+ ;
    > tAtom               ::= TYPE-IDENTIFIER
    >                       | IDENTIFIER
    >                       | tTuple
    >                       | '(' type ')' ;
    > tTuple              ::= '(' type (',' type)+ ')' ;

    === Top-level bindings

    > funStatements       ::= (funDeclaration | funDefinition)+ ;
    > funDeclaration      ::= IDENTIFIER ':' type ;
    > funDefinition       ::= IDENTIFIER pattern* '=' expression ;
    > gadtDeclaration     ::= 'data' IDENTIFIER 'where' (TYPE-IDENTIFIER ':' type)+ ;
    > adtDeclaration      ::= 'data' IDENTIFIER '=' adtConstructor ('|' adtConstructor)* ;
    > adtConstructor      ::= TYPE-IDENTIFIER tAtom+ ;
    > operatorFixity      ::= ('infixr' | 'infixl') INTEGER-LITERAL ('(' OPERATOR ')' | OPERATOR) ;
-}