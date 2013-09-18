{
  (* The type token is defined in parser.mli *)
  open Parser
}

let id = ['a'-'z''0'-'9''_']+

rule token = parse
| ['0'-'9']+ as i	{ INT (int_of_string i) }
| ['a'-'z''A'-'Z'] as c	{ CHAR c	}
| (id as s)':'		{ NAME s	}
| '^'			{ CARET		}
| '?'			{ QUESTION	}
| '*'			{ STAR		}
| '+' | '|'		{ PIPE		}
| "∩" | '&'		{ AND		}
| "¬" | '~'		{ NEGATE	}
| '('			{ LPAREN	}
| ')'			{ RPAREN	}
| '{'			{ LBRACE	}
| '}'			{ RBRACE	}
| ' '			{ token lexbuf	}

| eof			{ EOF		}
