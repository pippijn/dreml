open CorePervasives
open Types

let pat_simplify_list l =
  List.map Simplify.simplify_pat l


let pat_funs_simplify_list l =
  let (patterns, functions) = List.split l in
  List.combine (pat_simplify_list patterns) functions
  |> List.filter (
      not
      % Language.is_empty_language
      % Language.regex_of_pattern
      % fst
    )
