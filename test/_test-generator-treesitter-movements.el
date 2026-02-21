#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-treesitter-movements.el --- Generator for treesitter movement function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'cpo-treesitter-qd)

;; Generate tests for treesitter movement functions using diverse programming languages

;; JavaScript example with complex nesting
(carettest-tesmo-generate-tests
 "function processData(items) {
  const results = items.map(item => {
    if (item.type === 'special') {
      return {
        id: item.id,
        value: transform(item.data),
        metadata: {
          processed: true,
          timestamp: Date.now()
        }
      };
    }
    return item.id;
  });

  for (let i = 0; i < results.length; i++) {
    if (results[i].value > threshold) {
      console.log(`Processing item ${i}`);
    }
  }

  return results;
}"
 20  ; number of test positions
 '(cpo-treesitter-qd-up-to-parent-anchor-point
   cpo-treesitter-qd-up-to-root
   cpo-treesitter-qd-forward-sibling-anchor-point
   cpo-treesitter-qd-backward-sibling-anchor-point
   cpo-treesitter-qd-down-to-first-child-anchor-point
   cpo-treesitter-qd-down-to-last-child-anchor-point
   cpo-treesitter-qd-forward-inorder-traversal
   cpo-treesitter-qd-backward-inorder-traversal
   ("up-to-parent-2" (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2)))
   ("forward-sibling-2" (lambda () (cpo-treesitter-qd-forward-sibling-anchor-point 2)))
   ("backward-sibling-2" (lambda () (cpo-treesitter-qd-backward-sibling-anchor-point 2))))
 "test-treesitter-movements-javascript"
 :output-file "_generated-tests-treesitter-movements-javascript.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)

;; Python example with classes and functions
(carettest-tesmo-generate-tests
 "class DataProcessor:
    def __init__(self, config):
        self.config = config

    def process_items(self, items):
        for item in items:
            if item.get('value', 0) > 10:
                yield self.transform(item)

    def transform(self, item):
        return {'value': item['value'] * self.config, 'type': 'processed'}

def main():
    processor = DataProcessor(2.5)
    items = [{'value': 20}, {'value': 5}]
    for result in processor.process_items(items):
        print(result)
"
 15  ; number of test positions
 '(cpo-treesitter-qd-up-to-parent-anchor-point
   cpo-treesitter-qd-up-to-root
   cpo-treesitter-qd-forward-sibling-anchor-point
   cpo-treesitter-qd-backward-sibling-anchor-point
   cpo-treesitter-qd-down-to-first-child-anchor-point
   cpo-treesitter-qd-down-to-last-child-anchor-point
   cpo-treesitter-qd-forward-inorder-traversal
   cpo-treesitter-qd-backward-inorder-traversal
   ("up-to-parent-2" (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2)))
   ("forward-sibling-2" (lambda () (cpo-treesitter-qd-forward-sibling-anchor-point 2)))
   ("backward-sibling-2" (lambda () (cpo-treesitter-qd-backward-sibling-anchor-point 2))))
 "test-treesitter-movements-python"
 :output-file "_generated-tests-treesitter-movements-python.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.4
 :transient-mark-mode-prob 0.8)

;; Rust example with structs and traits
(carettest-tesmo-generate-tests
 "use std::collections::HashMap;

#[derive(Debug)]
struct DataItem {
    id: u32,
    value: String,
}

impl DataItem {
    fn new(id: u32, value: String) -> Self {
        DataItem { id, value }
    }

    fn process(&self) -> HashMap<&str, String> {
        vec![(\"id\", self.id.to_string()), (\"value\", self.value.clone())]
            .into_iter().collect()
    }
}
"
 12  ; number of test positions
 '(cpo-treesitter-qd-up-to-parent-anchor-point
   cpo-treesitter-qd-up-to-root
   cpo-treesitter-qd-forward-sibling-anchor-point
   cpo-treesitter-qd-backward-sibling-anchor-point
   cpo-treesitter-qd-down-to-first-child-anchor-point
   cpo-treesitter-qd-down-to-last-child-anchor-point
   cpo-treesitter-qd-forward-inorder-traversal
   cpo-treesitter-qd-backward-inorder-traversal
   ("up-to-parent-2" (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2)))
   ("forward-sibling-2" (lambda () (cpo-treesitter-qd-forward-sibling-anchor-point 2)))
   ("backward-sibling-2" (lambda () (cpo-treesitter-qd-backward-sibling-anchor-point 2))))
 "test-treesitter-movements-rust"
 :output-file "_generated-tests-treesitter-movements-rust.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.2
 :transient-mark-mode-prob 0.8)

;; Elisp example
(carettest-tesmo-generate-tests
 "
(defun validate-data-item (item)
  \"Validate that ITEM has required structure.\"
  (and (listp item)
       ;; ...
       (or (plist-get item :id)
           (and (consp item) (> (length item) 0)))))

(defmacro with-data-processing (data-var &rest body)
  \"Execute BODY with DATA-VAR bound to processed data.\"
  `(let ((,data-var (process-data-list data-list #'transform-numeric-data)))
     ,@body))

;; Example usage

"
 22  ; number of test positions
 '(cpo-treesitter-qd-up-to-parent-anchor-point
   cpo-treesitter-qd-up-to-root
   cpo-treesitter-qd-forward-sibling-anchor-point
   cpo-treesitter-qd-backward-sibling-anchor-point
   cpo-treesitter-qd-down-to-first-child-anchor-point
   cpo-treesitter-qd-down-to-last-child-anchor-point
   cpo-treesitter-qd-forward-inorder-traversal
   cpo-treesitter-qd-backward-inorder-traversal
   ("up-to-parent-2" (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2)))
   ("forward-sibling-2" (lambda () (cpo-treesitter-qd-forward-sibling-anchor-point 2)))
   ("backward-sibling-2" (lambda () (cpo-treesitter-qd-backward-sibling-anchor-point 2))))
 "test-treesitter-movements-elisp"
 :output-file "_generated-tests-treesitter-movements-elisp.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.3
 :transient-mark-mode-prob 0.8)

;; C++ example
(carettest-tesmo-generate-tests
 "#include <vector>
#include <algorithm>

template<typename T>
class DataProcessor {
private:
    std::vector<T> data_;

public:
    void addItem(const T& item) {
        data_.push_back(item);
    }

    template<typename Predicate>
    std::vector<T> filter(Predicate pred) const {
        std::vector<T> result;
        std::copy_if(data_.begin(), data_.end(),
                     std::back_inserter(result), pred);
        return result;
    }

    size_t size() const { return data_.size(); }
};
"
 13  ; number of test positions
 '(cpo-treesitter-qd-up-to-parent-anchor-point
   cpo-treesitter-qd-up-to-root
   cpo-treesitter-qd-forward-sibling-anchor-point
   cpo-treesitter-qd-backward-sibling-anchor-point
   cpo-treesitter-qd-down-to-first-child-anchor-point
   cpo-treesitter-qd-down-to-last-child-anchor-point
   cpo-treesitter-qd-forward-inorder-traversal
   cpo-treesitter-qd-backward-inorder-traversal
   ("up-to-parent-2" (lambda () (cpo-treesitter-qd-up-to-parent-anchor-point 2)))
   ("forward-sibling-2" (lambda () (cpo-treesitter-qd-forward-sibling-anchor-point 2)))
   ("backward-sibling-2" (lambda () (cpo-treesitter-qd-backward-sibling-anchor-point 2))))
 "test-treesitter-movements-cpp"
 :output-file "_generated-tests-treesitter-movements-cpp.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.25
 :transient-mark-mode-prob 0.8)


