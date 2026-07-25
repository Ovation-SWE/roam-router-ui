;;; roam-router-ui.el --- roam-router integration for org-roam-ui -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2024 Ovation-SWE

;; Author: Ovation-SWE
;; URL: https://github.com/Ovation-SWE/roam-router-ui
;; Keywords: files outlines
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0") (org-roam-ui "0.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Extends org-roam-ui with roam-router hierarchy links.
;;
;; When roam-router is installed and nodes are named using dot-notation
;; (e.g. school.csci-2400.concept.memoization), this package computes
;; parent-child links based on path prefixes and injects them into the
;; org-roam-ui graph as links of type "roam-router-hierarchy".
;;
;; Usage:
;;   (require 'roam-router-ui)
;;   (roam-router-ui-mode 1)
;;
;; The mode sets `org-roam-ui-extra-links-function' so that every time
;; org-roam-ui sends graph data to the browser, hierarchy links are
;; included alongside the standard org-roam links.

;;; Code:

(require 'org-roam)
(require 'org-roam-ui)

(defgroup roam-router-ui nil
  "roam-router integration for org-roam-ui."
  :group 'org-roam-ui
  :prefix "roam-router-ui-")

(defun roam-router-ui--build-path-map ()
  "Return a hash-table mapping roam-router path strings to org-roam node IDs.

Only file-level nodes (level 0) whose files live directly in
`org-roam-directory' (not in subdirectories) are included, since
roam-router uses a flat naming convention."
  (let ((nodes (org-roam-db-query
                [:select [id file]
                 :from nodes
                 :where (= level 0)]))
        (path-map (make-hash-table :test 'equal))
        (roam-dir (file-name-as-directory (expand-file-name org-roam-directory))))
    (dolist (node nodes)
      (let* ((id (car node))
             (file (cadr node)))
        (when (string= (file-name-directory file) roam-dir)
          (puthash (file-name-base file) id path-map))))
    path-map))

(defun roam-router-ui--find-parent-id (segs path-map)
  "Return the ID of the closest ancestor of SEGS found in PATH-MAP.

SEGS is a list of path segments (strings) for the child node.
Tries progressively shorter prefixes from length N-1 down to 1,
returning the first match or nil if none exists."
  (let ((n (length segs))
        (found nil))
    (while (and (> n 1) (not found))
      (setq n (1- n))
      (let ((prefix (mapconcat #'identity (seq-take segs n) ".")))
        (when-let ((id (gethash prefix path-map)))
          (setq found id))))
    found))

(defun roam-router-ui--get-hierarchy-links ()
  "Return hierarchy links for all roam-router-managed nodes.

Queries the org-roam database for file-level nodes in
`org-roam-directory', filters to those matched by a roam-router
pattern, and returns a list of (CHILD-ID PARENT-ID \"roam-router-hierarchy\")
triples suitable for consumption by `org-roam-ui-extra-links-function'."
  (unless (fboundp 'roam-router--load-patterns)
    (user-error "roam-router is not loaded; cannot compute hierarchy links"))
  (let* ((patterns (roam-router--load-patterns))
         (path-map (roam-router-ui--build-path-map))
         (nodes (org-roam-db-query
                 [:select [id file]
                  :from nodes
                  :where (= level 0)]))
         (roam-dir (file-name-as-directory (expand-file-name org-roam-directory))))
    (delq nil
          (mapcar
           (lambda (node)
             (let* ((id (car node))
                    (file (cadr node)))
               (when (string= (file-name-directory file) roam-dir)
                 (let ((path (file-name-base file)))
                   (when (roam-router--match-path path patterns)
                     (let* ((segs (split-string path "\\."))
                            (parent-id (roam-router-ui--find-parent-id segs path-map)))
                       (when parent-id
                         (list id parent-id "roam-router-hierarchy"))))))))
           nodes))))

;;;###autoload
(define-minor-mode roam-router-ui-mode
  "Enable roam-router hierarchy links in org-roam-ui.

When active, sets `org-roam-ui-extra-links-function' so that
parent-child links derived from roam-router dot-notation filenames
are included in the org-roam-ui graph.  These appear in the browser
as toggleable \"Roam-Router Hierarchy\" links."
  :lighter " rr-ui"
  :global t
  :group 'roam-router-ui
  :init-value nil
  (if roam-router-ui-mode
      (setq org-roam-ui-extra-links-function #'roam-router-ui--get-hierarchy-links)
    (when (eq org-roam-ui-extra-links-function #'roam-router-ui--get-hierarchy-links)
      (setq org-roam-ui-extra-links-function nil))))

(provide 'roam-router-ui)
;;; roam-router-ui.el ends here
