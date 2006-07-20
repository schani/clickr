;;; clickr-utils.lisp --- Various Clickr add-ons and tools

;; Copyright (C) 2006 Mark Probst

;; Author: Mark Probst <mark.probst@gmail.com>
;; Maintainer: Mark Probst <mark.probst@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

(defpackage "CLICKR-UTILS"
  (:use "CL" "CCL" "FLICKR" "CLICKR")
  (:export #:photo-biggest-size #:user-background-favorite-sizes
	   #:user-favorite-owner-threshold #:unused-groups #:unused-groups-html-string))

(in-package :clickr-utils)

(defun flickr-size-pixels (size)
  (* (flickr-size-width size) (flickr-size-height size)))

(defun photo-biggest-size (photo)
  (let* ((sizes (photo-sizes photo))
	 (sorted-sizes (sort sizes #'(lambda (a b)
				       (< (flickr-size-pixels a) (flickr-size-pixels b))))))
    (last sorted-sizes)))

(defun flickr-size-background-size-p (size)
  (and (>= (flickr-size-width size) 1024)
       (>= (flickr-size-height size) 768)))

(defun user-background-favorite-sizes (user)
  (let* ((favorites (user-favorites user))
	 (biggest-sizes (mapcar #'photo-biggest-size favorites)))
    (remove-if-not #'flickr-size-background-size-p biggest-sizes)))

(defun user-favorite-owner-threshold (user threshold)
  (let* ((owners (mapcar #'photo-owner (user-favorites user)))
	 (unique-owners (remove-duplicates owners)))
    (remove-if-not #'(lambda (o)
		       (>= (count o owners) threshold))
		   unique-owners)))

(defun unused-groups (user threshold)
  (let ((groups (user-groups user))
	(photos (user-photos user)))
    (remove-if #'(lambda (g)
		   (let ((group-photos (remove-if-not #'(lambda (p)
							  (member g (photo-groups p)))
						      photos)))
		     (>= (length group-photos) threshold)))
	       groups)))

(defun unused-groups-html-string (user threshold)
  (let* ((groups (unused-groups user threshold))
	 (group-strings (mapcar #'(lambda (g)
				    (format nil "<p><a href=\"~A\">~A</a></p>" (group-url g) (group-title g)))
				groups)))
    (format nil "<html><body>~%~{~A~%~}</body></html>~%" group-strings)))
