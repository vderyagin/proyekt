;;; proyekt.el --- Project-related helpers -*- lexical-binding: t -*-

;; Copyright (C) 2023 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Maintainer: Victor Deryagin <vderyagin@gmail.com>
;; Created: 16 Dec 2023
;; Version: 0.1.0

;; Package-Requires: ((consult))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'map)
(require 'project)
;; (require 'consult)

(eval-when-compile
  (require 'subr-x))

(defvar proyekt-cache (make-hash-table :test #'equal))

(cl-defun proyekt-add-command-set (name &key items items-fn predicate)
  (cl-assert (and (xor (and items (listp items))
                       (functionp items-fn))
                  (functionp predicate)
                  (stringp name)))
  (map-put! proyekt-cache name
            (list
             :items (or items-fn (lambda () items))
             :predicate predicate)))

(defun proyekt-lookup-command (candidates name)
  (seq-find
   (lambda (candidate) (string= (plist-get candidate :name) name))
   candidates))

(defun proyekt-run-command (command)
  (let ((action (plist-get command :action)))
    (funcall (pcase-exhaustive action
               ((pred stringp) #'compile)
               ((pred functionp) #'funcall))
             action)))

(defun proyekt-annotate (command)
  (let ((desc (plist-get command :description))
        (action (plist-get command :action)))
    (or desc
        (and (stringp action) (format "(%s)" action)))))

(defun proyekt-make-source (name command-set)
  (when-let* (((funcall (plist-get command-set :predicate)))
              (items (funcall (plist-get command-set :items)))
              (root default-directory))
    (list
     :category nil
     :items (seq-map (lambda (command) (plist-get command :name)) items)
     :name name
     :annotate (lambda (name)
                 (proyekt-annotate (proyekt-lookup-command items name)))
     :action (lambda (name)
               (let ((default-directory root))
                 (proyekt-run-command (proyekt-lookup-command items name)))))))

(defun proyekt-sources (root)
  (let ((default-directory root))
    (thread-last
      proyekt-cache
      (map-apply #'proyekt-make-source)
      (seq-filter #'identity))))

(declare-function consult--multi "consult")
;;;###autoload
(defun proyekt-run ()
  (interactive)
  (when-let* ((root (project-root (project-current t))))
    (require 'consult)
    (consult--multi (proyekt-sources root))))

(proyekt-add-command-set
 "Gentoo overlay"
 :items '((:name "scan for QA issues" :action "pkgcheck scan")
          (:name "update manifests" :action "pkgdev manifest"))
 :predicate
 (lambda ()
   (and (or (executable-find "pkgcheck")
            (executable-find "pkgdev"))
        (file-regular-p "profiles/repo_name"))))

(proyekt-add-command-set
 "Node scripts"
 :items-fn
 (lambda ()
   (let ((json (with-current-buffer (or (find-buffer-visiting "./package.json")
                                        (find-file-noselect "./package.json"))
                 (save-excursion
                   (goto-char (point-min))
                   (json-parse-buffer)))))
     (seq-map
      (pcase-lambda (`(,name . ,cmd))
        (list
         :name name
         :description (format "(%s)" cmd)
         :action (format "npm run %s" name)))
      (map-pairs (map-elt json "scripts")))))
 :predicate
 (lambda ()
   (and (executable-find "npm")
        (file-regular-p "package.json"))))

(proyekt-add-command-set
 "Just"
 :items-fn (lambda ()
             (seq-map
              (lambda (c)
                (let ((results (split-string c "#" t (rx (+ space)))))
                  (list
                   :name (car results)
                   :description (cadr results)
                   :action (format "just %s" (car (split-string (car results) " " t))))))
              (string-lines (shell-command-to-string "just --list --list-prefix '' --list-heading ''") t)))
 :predicate
 (lambda ()
   (and (executable-find "just")
        (or (file-regular-p "Justfile")
            (file-regular-p ".justfile")
            (file-regular-p "justfile")))))

(proyekt-add-command-set
 "Cargo"
 :items '((:name "build" :action "cargo build")
          (:name "test" :action "cargo test")
          (:name "release build" :action "cargo build --release")
          (:name "run benchmarks" :action "cargo bench")
          (:name "update dependencies" :action "cargo update")
          (:name "show outdated dependencies" :action "cargo outdated")
          (:name "run" :action "cargo run")
          (:name "check & report errors" :action "cargo check")
          (:name "remove 'target' directory" :action "cargo clean"))
 :predicate
 (lambda ()
   (and (executable-find "cargo")
        (file-regular-p "Cargo.toml"))))

(proyekt-add-command-set
 "Mix"
 :items '((:name "build" :action "mix compile")
          (:name "test" :action "mix test")
          (:name "get dependencies" :action "mix deps.get")
          (:name "delete generated application files" :action "mix clean")
          (:name "list outdated dependencies" :action "mix hex.outdated --all")
          (:name "run dialyzer" :action "mix dialyzer")
          (:name "run Elixir formatter" :action "mix format")
          (:name "run arbitrary mix command"
                 :action (lambda ()
                           (thread-last
                             "Command: "
                             read-string
                             (format "mix %s")
                             compile))))
 :predicate
 (lambda ()
   (and (executable-find "mix")
        (file-regular-p "mix.exs"))))


(proyekt-add-command-set
 "Cask"
 :items '((:name "build" :action "cask build")
          (:name "install depencencies" :action "cask install")
          (:name "update dependencies" :action "cask update")
          (:name "cleanup bytecode" :action "cask clean-elc")
          (:name "list dependencies" :action "cask list")
          (:name "eval expression"
                 :action (lambda ()
                           (thread-last
                             "Expression: "
                             read--expression
                             prin1-to-string
                             (format "cask eval '(message \"%%s\" %s)'")
                             compile))))
 :predicate
 (lambda ()
   (and (executable-find "cask")
        (file-regular-p "Cask"))))

(proyekt-add-command-set
 "Bundle"
 :items '((:name "install dependencies" :action "bundle install")
          (:name "update dependencies" :action "bundle update")
          (:name "list outdated dependencies" :action "bundle outdated"))
 :predicate
 (lambda ()
   (and (executable-find "bundle")
        (file-regular-p "Gemfile"))) )

(proyekt-add-command-set
 "NPM"
 :items '((:name "test" :action "npm test")
          (:name "update depencencies" :action "npm update")
          (:name "list outdated packages" :action "npm outdated")
          (:name "install dependencies" :action "npm install")
          (:name "add runtime dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "npm install --save %s")
                             compile)))
          (:name "add development dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "npm install --save-dev %s")
                             compile))))
 :predicate
 (lambda ()
   (and (executable-find "npm")
        (file-regular-p "package.json"))))

(proyekt-add-command-set
 "Yarn"
 :items '((:name "install dependencies" :action "yarn --no-emoji --no-progress")
          (:name "update depencencies" :action "yarn upgrade --no-emoji --no-progress")
          (:name "list outdated dependencies" :action "yarn outdated")
          (:name "add runtime dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "yarn add --no-emoji --no-progress %s")
                             compile)))
          (:name "add development dependency"
                 :action (lambda ()
                           (thread-last
                             "Package: "
                             read-string
                             (format "yarn add --dev --no-emoji --no-progress  %s")
                             compile))))
 :predicate
 (lambda ()
   (and (executable-find "yarn")
        (file-regular-p "yarn.lock"))))

(proyekt-add-command-set
 "Sorbet"
 :items '((:name "check types" :action "srb typecheck"))
 :predicate
 (lambda ()
   (and (executable-find "srb")
        (file-regular-p "sorbet/config"))))

;;; proyekt.el ends here
