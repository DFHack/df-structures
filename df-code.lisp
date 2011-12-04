;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-xml)

(defun find-entity (key) (find-by-id $global.world.entities.all $id key))
(defun find-unit (key) (find-by-id $global.world.units.all $id key))
(defun find-item (key) (find-by-id $global.world.items.all $id key))
(defun find-nemesis (key) (find-by-id $global.world.nemesis.all $id key))
(defun find-artifact (key) (find-by-id $global.world.artifacts.all $id key))
(defun find-building (key) (find-by-id $global.world.buildings.all $id key))

(defun material-by-id (mat-type &optional mat-idx)
  (let ((raws $global.world.raws))
    (cond ((= mat-type 0)
           (or $raws.inorganics[mat-idx].material
               $raws.materials_other[0]))
          ((<= 19 mat-type 418)
           (or $raws.creatures[mat-idx].material[(- mat-type 19)]
               $raws.materials_other[19]))
          ((<= 419 mat-type 429) ; TODO: upper bound
           (or $raws.organics_all[mat-idx].material[(- mat-type 419)]
               $raws.materials_other[419]))
          ((< 0 mat-type)
           $raws.materials_other[mat-type]))))

(defun food-mat-by-idx (category idx)
  (let* ((raws $global.world.raws)
         (table $raws.food_mat_table)
         (type $table.types[category][idx])
         (idx $table.indexes[category][idx]))
    (case category
      ((1 2 3)
       $raws.creatures[type].caste[idx])
      (otherwise
       (material-by-id type idx)))))
