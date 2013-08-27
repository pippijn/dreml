%{
open Types
%}

%parameter<Lbl : Types.LabelType>

%token EOF
%token LPAREN RPAREN LBRACE RBRACE PIPE AND STAR NEGATE CARET QUESTION

%token<char> CHAR
%token<int> INT
%token<string> NAME

%start<Lbl.t Types.pattern> start

%%

start:
	| pattern EOF
	  { $1 }


pattern:
	| pat_and
	  { $1 }
	| pattern PIPE pat_and
	  { PatChoice ($1, $3) }


pat_and:
	| pat_branch
	  { $1 }
	| pat_and AND pat_branch
	  { PatIntersect ($1, $3) }


pat_branch:
	| pat_expression
	  { $1 }
	| NEGATE pat_expression
	  { PatNot $2 }
	| pat_branch pat_expression
	  { PatConcat ($1, $2) }


pat_expression:
	| LPAREN NAME regex RPAREN
	  { VarBase (Lbl.make $2, $3) }
	| LPAREN NAME pattern RPAREN
	  { VarGroup (Lbl.make $2, $3) }
	| LPAREN pattern RPAREN
	  { $2 }
	| pat_expression STAR
	  { PatStar ($1) }
	| r=pat_expression CARET n=INT
	| r=pat_expression LBRACE n=INT RBRACE
	  { PatRepeat (r, n) }


regex:
	| re_and
	  { $1 }
	| regex PIPE re_and
	  { Choice ($1, $3) }

re_and:
	| re_branch
	  { $1 }
	| re_and AND re_branch
	  { Intersect ($1, $3) }


re_branch:
	| re_expression
	  { $1 }
	| NEGATE re_expression
	  { Not $2 }
	| re_branch re_expression
	  { Concat ($1, $2) }

re_expression:
	| LPAREN regex RPAREN
	  { $2 }
	| re_expression STAR
	  { Star ($1) }
	| re_expression QUESTION
	  { Choice (Epsilon, $1) }
	| r=re_expression CARET n=INT
	| r=re_expression LBRACE n=INT RBRACE
	  { Repeat (r, n) }
	| CHAR
	  { Letter ($1) }
