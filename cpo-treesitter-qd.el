;; -*- lexical-binding: t; -*-

(require 'cpo-tree-walk)
(require 'treesit)

;; TODO - how to use treesitter
;; The built-in treesitter support in emacs 29 is in the treesit group.
;; A buffer needs to be activated with (treesit-parser-create LANG-SYM), which attaches it to buffer, and includes it in the list (treesit-parser-list) for the buffer.
;; After buffer initialization, you can use (treesit-node-at POINT) to get leaf nodes (usually not what you actually want) and (treesit-node-on REGION-B REGION-E) to get the smallest node that covers the region.
;; Once you have a node, you can use treesit-node-parent, treesit-node-children, treesit-node-child (to select Nth or by name), treesit-node-start/end, treesit-node-type
;;
;; TODO - how to generically get interesting nodes?  Nodes with types like "(" and ")" are basically just lexer literals, but without a list of them, it's unclear how to differentiate that from eg. "symbol" type (in elisp) that is actually useful.  When printing a node, the printed representation seems to have symbol types sometimes and string types at other times, in a way that seems useful.  But treesit-node-type always returns a string!  But maybe it only looks like a symbol at times due to quirks of how it is presented.
;; TODO - so I think I need per-parser configuration for what nodes are interesting in what contexts.
;;
;; I'm going to write a trial generic handler as cpo-treesitter-qd-*

(defvar-local cpo-treesitter-qd-splicing-rules nil
  "
UNSTABLE - I may change the type of this.

Buffer-local list of rules for splicing treesitter nodes.
Each rule is a list (CHILD-TYPE PARENT-TYPE) specifying that nodes
of CHILD-TYPE should be spliced into their parent when the parent
has type PARENT-TYPE. When a node matches a splicing rule, its
children are treated as direct children of its parent node.

Example: '((\"parameter_list\" \"function_call\"))
This would cause parameter_list nodes inside function_call nodes
to be spliced, so f(a, b, c) appears as (function f \"(\" a b c \")\")
instead of (function (parameter_list a b c)).

To configure splicing for a buffer, set this variable locally:
  (setq-local cpo-treesitter-qd-splicing-rules
              '((\"parameter_list\" \"function_call\")
                (\"argument_list\" \"call_expression\")))

This affects all navigation functions including parent/child/sibling
navigation and bounds calculation.")

(defvar-local cpo-treesitter-qd-uninteresting-node-type-list
    '("(" ")"
      "{" "}"
      "[" "]"
      "\""
      "'"
      "+"
      "-"
      "*"
      "/"
      "~"
      "!"
      "="
      "!="
      "<"
      ">"
      "<="
      ">="
      "+="
      "-="
      "/="
      "*="
      "."
      ","
      ":"
      "::"
      ";"
      "&"
      "&&"
      "|"
      "||"
      "^"
      "~"
      "@"
      "#"
      "$"
      "%"
      "function"
      "const"
      "var"
      "let"
      "if"
      "else"
      "elif"
      "fi"
      "end"
      "while"
      "for"
      "do"
      "in"
      "of"
      "switch"
      "continue"
      "break"
      "return"
      "fn"
      "crate"
      "enum"
      "extern"
      "impl"
      "loop"
      "match"
      "mod"
      "move"
      "pub"
      "ref"
      "static"
      "struct"
      "type"
      "unsafe"
      "use"
      "where"
      "async"
      "awayt"
      "dyn"
      "after"
      "and"
      "andalso"
      "band"
      "begin"
      "bnot"
      "bor"
      "bsl"
      "bsr"
      "bxor"
      "case"
      "catch"
      "try"
      "cond"
      "div"
      "end"
      "fun"
      "not"
      "or"
      "orelse"
      "receive"
      "rem"
      "when"
      "xor"
      "auto"
      "alignas"
      "alignof"
      "constexpr"
      "default"
      "goto"
      "inline"
      "register"
      "restrict"
      "signed"
      "switch"
      "typedef"
      "typeof"
      "typeof_unqual"
      "unison"
      "volatile"
      "def"
      "as"
      "del"
      "assert"
      "class"
      "except"
      "finally"
      "from"
      "global"
      "nonlocal"
      "import"
      "is"
      "lambda"
      "pass"
      "raise"
      "with"
      "yield"
      )
  "List of Treesitter node types (as strings) that are “uninteresting”.
Typically this means that they are syntactic elements that can be used as anchors that can uniquely identify a syntax node.
This is WIP stuff that doesn't fully work.
There are certainly improvements to be made in terms of generically using treesitter, but also you can get better results by writing code that is more specific to a particular treesitter grammar.
But that is more work.
")

(defun cpo-treesitter-qd--node-should-be-spliced-p (node)
  "Return non-nil if NODE should be spliced into its parent.
Checks if NODE's type and its parent's type match any splicing rule."
  (when (and node cpo-treesitter-qd-splicing-rules)
    (let ((node-type (treesit-node-type node))
          (parent (treesit-node-parent node)))
      (when parent
        (let ((parent-type (treesit-node-type parent)))
          (seq-some (lambda (rule)
                      (and (equal (car rule) node-type)
                           (equal (cadr rule) parent-type)))
                    cpo-treesitter-qd-splicing-rules))))))

(defun cpo-treesitter-qd--effective-parent (node)
  "Get the effective parent of NODE, accounting for splicing.
If NODE's parent should be spliced into its grandparent, returns the grandparent.
Otherwise returns the actual parent."
  (let ((parent (treesit-node-parent node)))
    (if (and parent (cpo-treesitter-qd--node-should-be-spliced-p parent))
        (cpo-treesitter-qd--effective-parent parent)
      parent)))

(defun cpo-treesitter-qd--effective-children (node)
  "Get the effective children of NODE, accounting for splicing.
Any child that should be spliced will have its children included
in place of itself in the returned list."
  (let ((result '()))
    (dolist (child (reverse (treesit-node-children node)))
      (if (cpo-treesitter-qd--node-should-be-spliced-p child)
          (setq result (append (cpo-treesitter-qd--effective-children child)
                               result))
        (push child result)))
    result))

(defun cpo-treesitter-qd--next-effective-sibling (node fwd)
  "Get the next effective sibling of NODE in direction FWD.
If FWD is non-nil, gets the next sibling, otherwise gets previous.
Accounts for splicing by using effective siblings."
  (let* ((effective-parent (cpo-treesitter-qd--effective-parent node))
         (effective-siblings (when effective-parent
                               (cpo-treesitter-qd--effective-children effective-parent)))
         (current-pos (when effective-siblings
                        (seq-position effective-siblings node)))
         (next-pos (when current-pos
                     (if fwd (1+ current-pos) (1- current-pos)))))
    (when (and next-pos
               (>= next-pos 0)
               (< next-pos (length effective-siblings)))
      (nth next-pos effective-siblings))))

(defun cpo-treesitter-qd-node-interesting-p (node)
  (and node
       (let ((type (treesit-node-type node)))
         (not (or (member type cpo-treesitter-qd-uninteresting-node-type-list)
                  ;; TODO - checking if the type equals the string cat catch a bunch of things that the list can't, but it also catches things like symbols with text "symbol"...
                  ;;(equal type (treesit-node-string node))
                  )))))

(defun cpo-treesitter-qd--effective-parent-until (node pred include-node)
  (treesit-parent-until
   node
   (lambda (n) (and (not (cpo-treesitter-qd--node-should-be-spliced-p n))
                    (funcall pred n)))
   include-node))

(defun cpo-treesitter-qd-node-at-point (&optional pt)
  (let* ((pt (or pt (point)))
         (n (treesit-node-at (point))))
    (and n (cpo-treesitter-qd--effective-parent-until
            n
            #'cpo-treesitter-qd-node-interesting-p
            'include-node))))

(defun cpo-treesitter-qd--node-on (beg end)
  "Get the smallest interesting node that covers the region from BEG to END.
Like treesit-node-on but accounts for splicing and interesting nodes."
  (let ((n (treesit-node-on beg end)))
    (and n (cpo-treesitter-qd--effective-parent-until
            n
            #'cpo-treesitter-qd-node-interesting-p
            'include-node))))

(defun cpo-treesitter-qd-bounds-of-thing-at-point (&optional pt)
  (let* ((pt (or pt (point)))
         (n (cpo-treesitter-qd-node-at-point pt)))
    (and n (cons (treesit-node-start n)
                 (treesit-node-end n)))))

(defun cpo-treesitter-qd--qd-bounds-of-children-area (node)
  (let* ((first-interesting (cpo-treesitter-qd--qd-first-interesting-child node))
         (left-out-of-bounds (treesit-node-prev-sibling first-interesting))
         (last-interesting (cpo-treesitter-qd--qd-last-interesting-child node))
         (right-out-of-bounds (treesit-node-next-sibling last-interesting)))
    (cons (if left-out-of-bounds (treesit-node-end left-out-of-bounds) (treesit-node-start node))
          (if right-out-of-bounds (treesit-node-start right-out-of-bounds) (treesit-node-end node)))))

(defun cpo-treesitter-qd-bounds-of-thing-at-point/children-region (&optional pt)
  (let* ((pt (or pt (point)))
         (n (cpo-treesitter-qd-node-at-point pt)))
    (and n (cpo-treesitter-qd--qd-bounds-of-children-area n))))

(defun cpo-treesitter-qd-node-anchor-point (node)
  "Return an anchor point for the node, where hopefully running `cpo-treesitter-qd-bounds-of-thing-at-point' at the anchor point will return the same node.
But this is a heuristic thing, so we'll see if it works well."
  ;; My guess is that the first uninteresting node is going to be an anchor point.  A node starts with something uninteresting for a parenthesized list.
  ;; TODO - but higher order application could start with a paren list as the function, then parens for application.  There is no anchor point I can use without also inspecting the parent.  I need something like a list of parent types for the current parser that override child types, but also it should only override some child types, probably...  This is tough to do generically.
  (if (zerop (treesit-node-child-count node))
      (treesit-node-start node)
    (let ((uninteresting (seq-find
                          (lambda (x) (not (cpo-treesitter-qd-node-interesting-p x)))
                          (cpo-treesitter-qd--effective-children node))))
      (if uninteresting
          (treesit-node-start uninteresting)
        nil))))
;; TODO - This is working well for elisp, but I would like an end anchor point as well as a start anchor point.
;; TODO - autoloads

;;;###autoload (autoload 'cpo-treesitter-qd-up-to-parent-anchor-point "cpo-treesitter-qd.el" "" t)
(defun cpo-treesitter-qd-up-to-parent-anchor-point (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (n (cpo-treesitter-qd-node-at-point))
         (parent n)
         (_parent-set (dotimes (i count)
                        (and parent
                             (setq parent
                                   (cpo-treesitter-qd--effective-parent
                                    parent)))))
         (parent-anchor (and parent (cpo-treesitter-qd-node-anchor-point parent))))
    (and parent-anchor (goto-char parent-anchor))))

(defun cpo-treesitter-qd--qd-next-interesting-sibling (node fwd)
  (let ((sib (cpo-treesitter-qd--next-effective-sibling node fwd)))
    (while (and sib
                (not (cpo-treesitter-qd-node-interesting-p sib)))
      (setq sib (cpo-treesitter-qd--next-effective-sibling sib fwd)))
    sib))

;;;###autoload (autoload 'cpo-treesitter-qd-forward-sibling-anchor-point "cpo-treesitter-qd.el" "" t)
(defun cpo-treesitter-qd-forward-sibling-anchor-point (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count))
         (n (cpo-treesitter-qd-node-at-point))
         (sib n)
         (_sib-set (dotimes (i count)
                     (setq sib (cpo-treesitter-qd--qd-next-interesting-sibling sib fwd)))))
    (let ((anchor (and sib (cpo-treesitter-qd-node-anchor-point sib))))
      (and anchor (goto-char anchor)))))
;;;###autoload (autoload 'cpo-treesitter-qd-backward-sibling-anchor-point "cpo-treesitter-qd.el" "" t)
(defun cpo-treesitter-qd-backward-sibling-anchor-point (&optional count)
  (interactive "p")
  (cpo-treesitter-qd-forward-sibling-anchor-point (- (or count 1))))

(defun cpo-treesitter-qd--qd-first-interesting-child (node)
  (let ((children (cpo-treesitter-qd--effective-children node)))
    (seq-find #'cpo-treesitter-qd-node-interesting-p children)))

(defun cpo-treesitter-qd--qd-last-interesting-child (node)
  (let ((children (cpo-treesitter-qd--effective-children node)))
    (seq-find #'cpo-treesitter-qd-node-interesting-p (reverse children))))

;;;###autoload (autoload 'cpo-treesitter-qd-down-to-first-child-anchor-point "cpo-treesitter-qd.el" "" t)
(defun cpo-treesitter-qd-down-to-first-child-anchor-point ()
  (interactive)
  (let* ((n (cpo-treesitter-qd-node-at-point))
         (sib (and n (cpo-treesitter-qd--qd-first-interesting-child n)))
         (anchor (and sib (cpo-treesitter-qd-node-anchor-point sib))))
    (and anchor (goto-char anchor))))
;;;###autoload (autoload 'cpo-treesitter-qd-down-to-last-child-anchor-point "cpo-treesitter-qd.el" "" t)
(defun cpo-treesitter-qd-down-to-last-child-anchor-point ()
  (interactive)
  (let* ((n (cpo-treesitter-qd-node-at-point))
         (sib (and n (cpo-treesitter-qd--qd-last-interesting-child n)))
         (anchor (and sib (cpo-treesitter-qd-node-anchor-point sib))))
    (and anchor (goto-char anchor))))

;;;###autoload (autoload 'cpo-treesitter-qd-expand-region "cpo-treesitter-qd.el" "" t)
(defun cpo-treesitter-qd-expand-region ()
  "Expand the current region to the effective parent node. "
  (interactive)
  (let* ((current-node (if (region-active-p)
                           (cpo-treesitter-qd--node-on (region-beginning) (region-end))
                         (cpo-treesitter-qd-node-at-point)))
         (parent-node (and current-node
                           ;; If region wasn't active before, then we just get the current node.
                           (if (region-active-p)
                               (cpo-treesitter-qd--effective-parent current-node)
                             current-node))))
    (when parent-node
      (let ((bounds (cons (treesit-node-start parent-node)
                          (treesit-node-end parent-node))))
        (goto-char (car bounds))
        (set-mark (cdr bounds))
        (activate-mark)))))


(cpo-tree-walk-define-operations
 :def-inorder-forward cpo-treesitter-qd-forward-inorder-traversal
 :def-inorder-backward cpo-treesitter-qd-backward-inorder-traversal

 :def-expand-region-idempotent cpo-treesitter-qd-expand-region-idempotent
 :def-select-children-once cpo-treesitter-qd-select-children-region-idempotent
 :def-expand-region-to-children/ancestor-generation cpo-treesitter-qd-expand-region/children-region
 ;;:def-down-to-last-child cpo-treesitter-qd-down-to-last-child-beginning
 :def-transpose-sibling-forward cpo-treesitter-qd-transpose-sibling-forward
 :def-transpose-sibling-backward cpo-treesitter-qd-transpose-sibling-backward
 :def-ancestor-reorder cpo-treesitter-qd-ancestor-reorder
 :def-up-to-root cpo-treesitter-qd-up-to-root
 :def-select-root cpo-treesitter-qd-select-root

 :use-object-name "treesitter tree (via 'treesit.el', using quick-and-dirty cpo-treesitter-qd movement and selection)"

 :use-down-to-last-child 'cpo-treesitter-qd-down-to-last-child-anchor-point

 :use-up-to-parent 'cpo-treesitter-qd-up-to-parent-anchor-point
 :use-down-to-first-child 'cpo-treesitter-qd-down-to-first-child-anchor-point
 :use-next-sibling 'cpo-treesitter-qd-forward-sibling-anchor-point
 :use-previous-sibling 'cpo-treesitter-qd-backward-sibling-anchor-point
 :use-bounds 'cpo-treesitter-qd-bounds-of-thing-at-point
 :use-children-bounds 'cpo-treesitter-qd-bounds-of-thing-at-point/children-region
 )

;;;###autoload (autoload 'rmo/cpo-treesitter-qd-forward-inorder-traversal "cpo-treesitter-qd.el" "" t)
;;;###autoload (autoload 'rmo/cpo-treesitter-qd-backward-inorder-traversal "cpo-treesitter-qd.el" "" t)
;;;###autoload (autoload 'rmo/cpo-treesitter-qd-forward-sibling-anchor-point "cpo-treesitter-qd.el" "" t)
;;;###autoload (autoload 'rmo/cpo-treesitter-qd-backward-sibling-anchor-point "cpo-treesitter-qd.el" "" t)
;;;###autoload (autoload 'rmo/cpo-treesitter-qd-down-to-last-child-anchor-point "cpo-treesitter-qd.el" "" t)
(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-treesitter-qd-forward-inorder-traversal 'cpo-treesitter-qd-backward-inorder-traversal)
  (repeatable-motion-define-pair 'cpo-treesitter-qd-forward-sibling-anchor-point 'cpo-treesitter-qd-backward-sibling-anchor-point)
  (repeatable-motion-define-pair 'cpo-treesitter-qd-up-to-parent-anchor-point 'cpo-treesitter-qd-down-to-first-child-anchor-point)
  (repeatable-motion-define 'cpo-treesitter-qd-down-to-last-child-anchor-point 'cpo-treesitter-qd-up-to-parent-anchor-point)
  (repeatable-motion-define 'cpo-treesitter-qd-expand-region nil)
  (repeatable-motion-define 'cpo-treesitter-qd-expand-region-idempotent nil)
  (repeatable-motion-define 'cpo-treesitter-qd-select-children-region-idempotent nil)
  (repeatable-motion-define 'cpo-treesitter-qd-expand-region/children-region nil)
  )

(provide 'cpo-treesitter-qd)
