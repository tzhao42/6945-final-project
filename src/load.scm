(with-working-directory-pathname
 (directory-pathname (current-load-pathname))
 (lambda ()
   (load
    '("common/overrides"
      "common/utils"
      "common/indexes"
      "common/collections"
      "common/memoizers"
      "common/predicates"
      ;;"common/predicate-metadata" | this is overrided by "user-defined-types/predicates"
      "common/applicability"
      "common/generic-procedures"
      "common/pretty-printer"
      "common/operators"
      ;;"common/operations" | this is overrided by "user-defined-types/operations"
      "common/package"
      "common/predicate-counter"
      "common/trie"))
   (load
    '("user-defined-types/generics"
      "user-defined-types/tagging"
      "user-defined-types/predicates"
      "user-defined-types/templates"
      "user-defined-types/values"
      "user-defined-types/tags"
      "user-defined-types/functions"
      "user-defined-types/operations"
      "user-defined-types/instance-substrate"))
   (load
    '("graph/object"
      "graph/node"
      "graph/edge"
      "graph/graph"
      "graph/update"))
   (load
    '("molecules/molecules"))))
