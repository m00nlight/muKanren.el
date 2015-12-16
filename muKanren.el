;;; muKanren.el --- muKanren implement in elisp -*- lexical-binding: t -*-

;; Copyright © 2015 m00nlight
;; Authors : m00nlight <dot_wangyushi@yeah.net>

;; Keywords : relational, logic, language, muKanren
;; Package-Requires : ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Implement the muKanren languages describe in the paper as follows:
;; μKanren: A Minimal Functional Corefor Relational Programming
;; 
;; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


(defun var (c) (make-vector 1 c))
(defun var? (v) (vectorp v))
(defun var=? (u v) (equal (elt u 0) (elt v 0)))


(defun walk (u s)
  (let ((pr (and (var? u) (assoc-default u s (lambda (u v) (var=? u v)) nil))))
    (if pr
	(walk pr s)
      u)))


(defun ext-s (x v s)
  `((,x . ,v) . ,s))

;; since emacs listp will return true for nil, which will cause the infinit
;; recursion in unify, we need to make a proper pair? procedure to judge
;; whether it is a proper cons pair to avoid infinite recursion.

(defun pair? (xs)
  (and (listp xs) (not (null (car xs)))))

;; since emacs lisp does not distinguish between empty list '() and false, we
;; create some dummy empty substitution for use.
(setq empty-s
      (ext-s (var -1) 'dummy '()))

(setq empty-state
      `(,empty-s . 0))

(setq mzero '())
(defun unit (sc) (cons sc mzero))

(defun unify (u v s)
  (let ((u (walk u s))
	(v (walk v s)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((new-s (unify (car u) (car v) s)))
	(and new-s (unify (cdr u) (cdr v) new-s))))
     (t (and (equal u v) s)))))


(defun == (u v)
  (lambda (sc)
    (let ((s (unify u v (car sc))))
      (if s
	  (unit `(,s . ,(cdr sc)))
	mzero))))

(defun call-fresh
  (lambda (f)
    (let ((c (cdr sc)))
      ((f (var c)) `(,(car sc) . ,(+ c 1))))))

(defun mplus (stream1 stream2)
  (cond
   ((null stream1) stream2)
   ((functionp stream1) (lambda () (mplus stream2 (stream1))))
   (t (cons (car stream1) (mplus (cdr stream1) stream2)))))

(defun bind (stream g)
  (cond
   ((null? stream) mzero)
   ((functionp stream) (lambda () (bind (stream) g)))
   (t (mplus (g (car stream)) (bind (cdr stream) g)))))

(defun disj (g1 g2) (lambda (sc) (mplus (g1 sc) (g2 sc))))
(defun conj (g1 g2) (lambda (sc) (bind (g1 sc) g2)))


