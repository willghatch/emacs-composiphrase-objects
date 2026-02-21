#!/bin/sh
":"; exec emacs --batch -L "$(dirname "$0")/.." -L "$(dirname "$0")/dependencies/carettest" --load "$0" "$@" # ;;; _test-generator-indent-movements.el --- Generator for indent movement function tests -*- lexical-binding: t; mode: emacs-lisp; -*-

(require 'carettest-tesmo-generator)
(require 'carettest-tesmut-generator)
(require 'cpo-indent-tree)

;; Generate tests for indent tree movement functions with diverse examples

(setq indent-tree-movements
      '(cpo-indent-tree-forward-full-sibling
        cpo-indent-tree-backward-full-sibling
        cpo-indent-tree-forward-full-or-half-sibling
        cpo-indent-tree-backward-full-or-half-sibling
        cpo-indent-tree-forward-to-last-full-sibling
        cpo-indent-tree-backward-to-first-full-sibling
        cpo-indent-tree-inorder-traversal-forward
        cpo-indent-tree-inorder-traversal-backward
        cpo-indent-tree-up-to-parent
        cpo-indent-tree-up-to-root
        cpo-indent-tree-down-to-first-child
        cpo-indent-tree-down-to-last-child
        cpo-indent-tree-down-to-last-descendant
        ("forward-full-sibling-2" (lambda () (cpo-indent-tree-forward-full-sibling 2)))
        ("up-to-parent-2" (lambda () (cpo-indent-tree-up-to-parent 2)))
        ("down-to-first-child-2" (lambda () (cpo-indent-tree-down-to-first-child 2)))))

;; Structure specifically designed to exercise half-sibling vs full-sibling difference.
;; Many nodes have no full sibling but do have a half-sibling at a different indent level.
(carettest-tesmo-generate-tests
 "parent
  section-a
    item-one
    item-two
  section-b
      section b half sibling here
    item-three
        half sibling here
      detail
        foo bar baz
      detail 2 here
grandparent
  branch-x
    leaf-alpha
  branch-y
    leaf-beta
    leaf-gamma"
 20  ; number of test positions
 indent-tree-movements
 "test-indent-movements-half-sibling"
 :output-file "_generated-tests-indent-movements-half-sibling.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.4
 :transient-mark-mode-prob 0.8)

;; Python code with multiple top-level definitions and complex indentation
(carettest-tesmo-generate-tests
 "#!/usr/bin/env python3
import os
import sys
from typing import List, Dict, Optional

class DatabaseConnection:
    def __init__(self, host: str, port: int):
        self.host = host
        self.port = port
        self.connected = False

    def connect(self):
        try:
            # Simulate connection logic
            if self.host and self.port > 0:
                self.connected = True
                return True
        except Exception as e:
            print(f\"Connection failed: {e}\")
            return False

    def disconnect(self):
        self.connected = False

    def execute_query(self, query: str) -> Optional[List[Dict]]:
        if not self.connected:
            raise RuntimeError(\"Not connected to database\")

        # Simulate query execution
        if query.lower().startswith('select'):
            return [
                {'id': 1, 'name': 'Alice'},
                {'id': 2, 'name': 'Bob'},
                {'id': 3, 'name': 'Charlie'}
            ]
        elif query.lower().startswith('insert'):
            return None
        else:
            raise ValueError(f\"Unsupported query: {query}\")

def process_user_data(users: List[Dict]) -> Dict[str, int]:
    \"\"\"Process user data and return statistics.\"\"\"
    stats = {
        'total_users': len(users),
        'active_users': 0,
        'admin_users': 0
    }

    for user in users:
        if user.get('is_active', False):
            stats['active_users'] += 1

        if user.get('role') == 'admin':
            stats['admin_users'] += 1

    return stats

def validate_config(config: Dict) -> bool:
    \"\"\"Validate configuration dictionary.\"\"\"
    required_keys = ['database_url', 'api_key', 'debug_mode']

    for key in required_keys:
        if key not in config:
            print(f\"Missing required config key: {key}\")
            return False

    if not isinstance(config['debug_mode'], bool):
        print(\"debug_mode must be a boolean\")
        return False

    return True

if __name__ == '__main__':
    config = {
        'database_url': 'localhost:5432',
        'api_key': 'secret123',
        'debug_mode': True
    }

    if validate_config(config):
        db = DatabaseConnection('localhost', 5432)

        if db.connect():
            try:
                users = db.execute_query('SELECT * FROM users')
                if users:
                    stats = process_user_data(users)
                    print(f\"User statistics: {stats}\")
            finally:
                db.disconnect()
    else:
        print(\"Invalid configuration\")
        sys.exit(1)
"
 10  ; number of test positions
 indent-tree-movements
 "test-indent-movements-python"
 :output-file "_generated-tests-indent-movements-python.el"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.4
 :transient-mark-mode-prob 0.8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mutators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq indent-tree-mutators
      '(cpo-indent-tree-demote
        cpo-indent-tree-promote
        cpo-indent-tree-open-sibling-forward
        cpo-indent-tree-open-sibling-backward
        cpo-indent-tree-transpose-sibling-forward
        cpo-indent-tree-transpose-sibling-backward
        ;; Sequence tests for complex operations
        (list cpo-indent-tree-demote cpo-indent-tree-open-sibling-forward)
        (list cpo-indent-tree-promote cpo-indent-tree-open-sibling-backward)))

;; Generate tests for indent tree modification functions
(carettest-tesmut-generate-tests
 "parent
  section-a
    item-one
    item-two
  section-b
      section b half sibling here
    item-three
        half sibling here
      detail
        foo bar baz
      detail 2 here
grandparent
  branch-x
    leaf-alpha
  branch-y
    leaf-beta
    leaf-gamma"
 20  ; number of test positions
 indent-tree-mutators
 "test-indent-modifications"
 :dest-dir (expand-file-name "generated-tests" (file-name-directory load-file-name))
 :file-name-random-replacement t
 :set-mark-prob 0.4
 :transient-mark-mode-prob 0.7)


