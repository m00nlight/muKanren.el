;;; muKanren-test.el -- muKanren unit test -*- lexical-binding: t -*-

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


(require 'ert)
(load-file "./muKanren.el")


(ert-deftest test-for-var? ()
  "Test for var? function..."
  (should (equal (var? (var 3)) t))
  (should (equal (var? 3) nil))
  (should (equal (var? "hello") nil)))

(ert-deftest test-for-var=? ()
  "Test for var=? function, it should return true if the two logical
variable are actually the same, nil otherwise"
  (should (equal (var=? (var 3) (var 3)) t))
  (should (equal (var=? (var 3) (var 4)) nil))
  (should (equal (var=? (var 2) (var 1)) nil))
  (should (equal (var=? (var 1) (var 1)) t)))


(ert-deftest test-for-walk ()
  "Walk should get value associate to logic variable in a substitutions
environment."
  (should (var=? (walk (var 3) empty-s) (var 3)))
  (should (equal (walk (var 3) (ext-s (var 3) 1 empty-s)) 1))
  (should (equal (walk (var 3) (ext-s (var 3) (var 1) (ext-s (var 1) 2
							     empty-s)))
		 2)))


(ert-deftest test-for-pair? ()
  "pair? should return true if the input is a cons pair, otherwies it 
should return nil. This function is written mainly to handle that 
emacs lisp listp function return true for nil(empty list), which will
yield infinite recursion in unfication."
  (should (equal (pair? '()) nil))
  (should (equal (pair? nil) nil))
  (should (equal (pair? '(1)) t))
  (should (equal (pair? '(1 2 3 4)) t))
  (should (equal (pair? '(1 2 3 . 4)) t)))


(ert-deftest test-for-unify ()
  "unify two expression and bind logical variable to value to make 
the two expression unifable."
  (should (equal (unify 2 3 empty-s) nil))
  (should (equal (unify 2 2 empty-s) empty-s))
  (should (equal (unify (var 0) 2 empty-s)
		 (ext-s (var 0) 2 empty-s)))
  (should (equal (unify 2 (var 0) empty-s)
		 (ext-s (var 0) 2 empty-s)))
  (should (equal (unify (list 1 2 3) (list 1 2 (var 0)) empty-s)
		 (ext-s (var 0) 3 empty-s))))



(ert-deftest test-for-== ()
  "== function is to use to see whether two logical variable 
can be unify, either return a answer(stream), or fail(nil)"
  (should (equal (funcall (== 1 1) empty-state) (unit empty-state)))
  (should (equal (funcall (== 1 2) empty-state) nil))
  (should (equal (funcall (== (var 0) 3) empty-state)
  		 (unit `(,(ext-s (var 0) 3 empty-s) . 0))))
  (should (equal (funcall (== (list 1 2 3) (list 1 2 (var 3))) empty-state)
		 (unit `(,(ext-s (var 3) 3 empty-s) . 0)))))







