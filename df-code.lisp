;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-defs)

(def (class* eas) df-flagarray (array-item data-field concrete-item)
  ()
  (:default-initargs :type-name $flag-bit))

(in-package :cl-linux-debug.data-info)

(defmethod compute-effective-fields ((type df-flagarray))
  (list
   (make-instance 'pointer :name $start)
   (make-instance 'int32_t :name $size)))

(defmethod array-base-dimensions ((type df-flagarray) ref)
  (assert (typep (effective-contained-item-of type) 'flag-bit))
  (let ((s $ref.start) (e $ref.size))
    (awhen (and s e)
      (values (start-address-of s) (* 8 e)))))

(in-package :cl-linux-debug.data-xml)

(defun find-entity (key) (find-by-id $global.world.entities.all $id key))
(defun find-unit (key) (find-by-id $global.world.units.all $id key))
(defun find-item (key) (find-by-id $global.world.items.all $id key))
(defun find-nemesis (key) (find-by-id $global.world.nemesis.all $id key))
(defun find-artifact (key) (find-by-id $global.world.artifacts.all $id key))
(defun find-building (key) (find-by-id $global.world.buildings.all $id key))
(defun find-activity (key) (find-by-id $global.world.activities.all $id key))
(defun find-squad (key) (find-by-id $global.world.squads.all $id key))

(defun find-creature (key) $global.world.raws.creatures[key])
(defun find-figure (key) (find-by-id $global.world.history.figures $id key))

(defun find-burrow (key) (find-by-id $global.ui.burrows.list $id key))

(defun material-by-id (mat-type &optional mat-idx)
  (let ((raws $global.world.raws))
    (cond ((= mat-type 0)
           (or $raws.inorganics[mat-idx].material
               $raws.mat_table.builtin[0]))
          ((<= 19 mat-type 218)
           (or $raws.creatures[mat-idx].material[(- mat-type 19)]
               $raws.mat_table.builtin[19]))
          ((<= 219 mat-type 418)
           (let ((hfig (find-figure mat-idx)))
             (values
              (or $raws.creatures[$hfig.race].material[(- mat-type 219)]
                  $raws.mat_table.builtin[19])
              hfig)))
          ((<= 419 mat-type 618)
           (or $raws.plants.all[mat-idx].material[(- mat-type 419)]
               $raws.mat_table.builtin[419]))
          ((< 0 mat-type)
           $raws.mat_table.builtin[mat-type]))))

(defun food-mat-by-idx (category-id idx)
  (let* ((raws $global.world.raws)
         (table $raws.mat_table)
         (category (enum-to-int $organic_mat_category category-id))
         (type $table.organic_types[category][idx])
         (idx $table.organic_indexes[category][idx]))
    (case category
      ((1 2 3)
       $raws.creatures[type].caste[idx])
      (otherwise
       (material-by-id type idx)))))

(defun describe-material ($)
  (let ((pfix (ignore-errors $.prefix))
        (mtemp $.heat.melting_point))
    (fmt "~@[~A ~]~A"
         (if (string= pfix "") nil pfix)
         (if (< mtemp 10015)
              $.state_name[1] $.state_name[0]))))

(defun item-subtype-target (type subtype)
  (let* ((defs $global.world.raws.itemdefs)
         (key (enum-to-key $item_type type))
         (table (case key
                  ($WEAPON $defs.weapons)
                  ($TRAPCOMP $defs.trapcomps)
                  ($TOY $defs.toys)
                  ($TOOL $defs.tools)
                  ($INSTRUMENT $defs.instruments)
                  ($ARMOR $defs.armor)
                  ($AMMO $defs.ammo)
                  ($SIEGEAMMO $defs.siege_ammo)
                  ($GLOVES $defs.gloves)
                  ($SHOES $defs.shoes)
                  ($SHIELD $defs.shields)
                  ($HELM $defs.helms)
                  ($PANTS $defs.pants)
                  ($FOOD $defs.food))))
    $table[subtype]))
