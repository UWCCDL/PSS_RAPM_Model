;;; This file contains an example of creating a new visual feature
;;; for the virtual window device included with ACT-R.  
;;;
;;; The implementation of the virtual windows is based on CLOS
;;; classes which can be found in the devices/virtual/device.lisp
;;; file.  Those classes will not be described in detail here and
;;; one should consult that code for documentation and implementation
;;; details.
;;;
;;; This code adds a new virtual dialog item called an image-vdi.
;;; It is intended to represent an image in the visual scene and
;;; provides the following simple feature and object representations
;;; to a model:
;;;
;;; Here is the visicon with the items created in the example model:
;;; Loc        Att   Kind           Value             Color           ID
;;; ---------  ---   -------------  ----------------  --------------  -------------
;;; ( 74 224)  NEW   IMAGE          "brain"           BLACK           VISUAL-LOCATION1
;;; (154  81)  NEW   IMAGE          "logo"            BLACK           VISUAL-LOCATION0
;;; 
;;; Here is the visual-location chunk for the first item:
;;; 
;;; VISUAL-LOCATION1-0-0
;;;    KIND  IMAGE
;;;    VALUE  IMAGE
;;;    COLOR  BLACK
;;;    HEIGHT  128
;;;    WIDTH  128
;;;    SCREEN-X  74
;;;    SCREEN-Y  224
;;;    DISTANCE  1080
;;;    SIZE  46.0
;;; 
;;; and here is the visual-object chunk for that item:
;;; 
;;; IMAGE1-0
;;;    SCREEN-POS  VISUAL-LOCATION1-0-0
;;;    VALUE  "brain"
;;;    COLOR  BLACK
;;;    HEIGHT  128
;;;    WIDTH  128
;;;    IMAGE  T


;;; The first step for creating a new virtual feature is to 
;;; create a subclass of the virtual-dialog-item class.
;;; Any new slots necessary for that class can be added, and
;;; it's recommended that each instance automatically create
;;; a unique id as shown here.

(defclass image-vdi (virtual-dialog-item)
  
  ;; add a new slot for this item which will be used to hold a string
  ;; specifying the name of a file to display if using the 
  ;; additional extension for the ACT-R Environment to extend
  ;; the visible-virtual-windows as well.
  
  ((file :accessor file :initform nil :initarg :file))
  
  (:default-initargs
    :id (new-name-fct "IMAGE-VDI") ;; make sure it gets a unique name
    :handles-click-p t             ;; indicate that this item can be clicked on by a model
    :action 'default-image-click)) ;; specify a default function to call when it is clicked

;;; Here we define the default function to call when
;;; an image-vdi feature is clicked.  It uses the vw-output
;;; command to print information in the model trace when the
;;; :vwt parameter in the model is set to t, and that will show
;;; the window, file, and text information from the image-vdi 
;;; item along with the position of the click.

(defmethod default-image-click ((self image-vdi) position)
  (vw-output (view-container self)
             "image with file ~s and text ~s clicked at relative position ~d ~d" 
             (file self) (dialog-item-text self) (px position) (py position)))

;;; The vv-click-event-handler is the method which must be written
;;; to actually process the click which the model produces.  This is
;;; the internal virtual window method that gets called and we use
;;; that to call the action function provided when the creation of
;;; an image-vdi item.  It computes the relative position of the click
;;; within the image and passes that to the action function.

(defmethod vv-click-event-handler ((self image-vdi) position)
  (when (or (functionp (action-function self))
            (and (symbolp (action-function self)) (fboundp (action-function self))))
    (funcall (action-function self) self (vector (- (px position) (x-pos self)) (- (py position) (y-pos self))))))

;;; The build-vis-locs-for method is typically written for the
;;; whole device, but the virtual windows in ACT-R extend that so
;;; that the device actually calls that method for each of the
;;; items in the window to create the features.  It needs to 
;;; return the chunk with the visual-location information desired.
;;;
;;; In this example it takes the extra step of being efficient and
;;; only creating a chunk when needed (one for each model which may be
;;; using it as the device) and then modifying that chunk each time
;;; the display is processed to provide the current information.
;;;
;;; It also sets some special parameters on that chunk to allow the
;;; vision module to automatically hide some information in the
;;; visual location chunk and fill in the details for the visual
;;; object.

(defmethod build-vis-locs-for ((self image-vdi) (vis-mod vision-module))
  ;; The chunks for the item are stored in the hash-table found in the
  ;; loc-chunks slot of the object which is created by default for all
  ;; virtual-dialog-item instances.  The hash-table uses an 'equal
  ;; test and the recommended key is a cons of the current meta-process
  ;; and current model to make sure that each model which may be using
  ;; this device gets its own chunk.  The values are lists of chunks,
  ;; which in this case is just a single chunk per item.
  (let* ((c (gethash (cons (current-mp) (current-model)) (loc-chunks self)))
         
         (f (cond ((and c (chunk-p-fct (car c)))
                   ;; have a chunk already so just modify the 
                   ;; standard visual-location slots with the current values from
                   ;; the image-vdi object
                   (mod-chunk-fct (car c) `(color ,(color self)
                                            screen-x ,(+ (x-pos self) (round (width self) 2))
                                            screen-y ,(+ (y-pos self) (round (height self) 2))
                                            width ,(width self)
                                            height ,(height self)))
                   (car c))
                  (t
                   ;; Since we are setting the value and kind slots to the symbol
                   ;; image we should make sure that is defined as a chunk otherwise
                   ;; the model will create a warning indicating that it has to
                   ;; do so by default.
                   
                   (unless (chunk-p image)
                     (define-chunks (image)))
                   
                   ;; Create the new chunk with the current item's values and
                   ;; store it in the table.  Here we set the value and kind
                   ;; slots both to the chunk image, but below we indicate that
                   ;; the 'real' value is actually the text of the item -- the
                   ;; real value is the one that a model can use to find a location
                   ;; but the resulting slot will always have the value image since
                   ;; content isn't supposed to be available until the item is attended.
                   
                   (car (setf (gethash (cons (current-mp) (current-model)) (loc-chunks self)) 
                          (define-chunks-fct `((isa visual-location
                                                    color ,(color self)
                                                    value image
                                                    kind image
                                                    screen-x ,(+ (x-pos self) (round (width self) 2))
                                                    screen-y ,(+ (y-pos self) (round (height self) 2))
                                                    width ,(width self)
                                                    height ,(height self))))))))))
    
    ;; Setting the visual-object parameter in a chunk which is
    ;; returned as a visual feature will cause the vision
    ;; vision module to call the vis-loc-to-obj method on
    ;; the value of that parameter instead of the device itself
    ;; to create the visual-object for the item.  This is convenient
    ;; when extending the system because it means that the parent
    ;; device's method does not have to be modified or extended in
    ;; some way.
    (setf (chunk-visual-object f) self)
    
    ;; Setting the real-visual-value parameter of a chunk which is 
    ;; returned as a visual feature lets the vision module know
    ;; what to show for the value slot of the item with print-visicon
    ;; if it should differ from the value which is in the value slot
    ;; of the chunk that gets set in the visual-location buffer.
    
    (setf (chunk-real-visual-value f) (dialog-item-text self))
    
    ;; return the visual location chunk
    f))
  

;;; Because we set the visual-object parameter of the visual-location
;;; chunk we can write a vis-loc-to-obj method for our image-vdi class
;;; to create the visual-object chunk when the item is attended.
;;; For our item we do not have any custom slots in the visual-object
;;; representation and thus we will use the fill-default-vis-obj-slots
;;; function to automatically set the color, height, and width slots
;;; based on the visual-location chunk along with the value slot
;;; based on the real-visual-value parameter if set, or the value
;;; slot of the visual-location chunk if it is not.

(defmethod vis-loc-to-obj ((self image-vdi) loc)
  
  ;; We are creating a sub-type of the visual-object chunk-type
  ;; which has a slot named image with a default value of t so that
  ;; one can use "isa image" in chunks and productions and have it
  ;; work basically like things used to (that's how the other default
  ;; features of text, buttons, and lines work).
  
  (unless (chunk-type-p image)
    (chunk-type (image (:include visual-object)) (image t)))
  
  ;; Create a new chunk and then just call fill-default-vis-obj-slots
  ;; to set the standard slots with the values from the visual-location
  ;; chunk.  If we wanted to include more information we could do so
  ;; before or after filling the default slots.
  
  (fill-default-vis-obj-slots (car (define-chunks (isa image))) loc))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The code above is sufficient for creating a new virtual dialog item ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The code below this point extends that virtual image dialog item to actually
;;; have it display a .gif file in a visible-virtual window in the ACT-R
;;; Environment in conjunction with the 999-creating-a-new-virtual-item.tcl
;;; file that extends the Environment interface.
;;;
;;; This code depends on internal mechanisms of the Environment connection
;;; which are not really documented or to be considered part of the actual API
;;; of the ACT-R software.  It is provided as a demonstration for those that
;;; are comfortable working with both Lisp and Tcl/Tk, and who are also 
;;; willing to extend the software in ways that may break under future 
;;; updates.


;;; The add-visual-items-to-rpm-window method on the visible-virtual-window class
;;; is responsible for sending the information to the Environment that it uses to
;;; display the subviews.  Because it wasn't originally built to be extended or 
;;; modified (you can't subclass the visible-virtual-window class since open-exp-window
;;; doesn't provide a way to do so and the original method assumes that all the 
;;; subviews are of known types) your best option is to just replace it with a
;;; version that contains the original items and any new ones you add.  At some
;;; point that may be refactored to provide an easy extension mechanism.


(defmethod add-visual-items-to-rpm-window ((win visible-virtual-window) &rest items)
  (dolist (item items)
    (add-subviews win item)
    (send-env-window-update 
     (case (type-of item)
       (env-button-vdi
        (list 'button (id win) (id item) (x-pos item) (y-pos item) 
          (width item) (height item) (dialog-item-text item) (color-symbol->env-color (color item))))
       (env-text-vdi
        (list 'text (id win) (id item) (x-pos item) (y-pos item) 
          (color-symbol->env-color (color item))  (dialog-item-text item) (round (text-height item) 10/12)))
       (env-line-vdi
        (list 'line (id win) (id item) (x-pos item) (y-pos item) 
              (color-symbol->env-color (color item)) (width item) (height item)))
       
       ;; add a case for the class of the new item and create the list of features
       ;; needed to display it in the Environment with the corresponding Tcl/Tk code
       ;; extension in 999-creating-a-new-virtual-item.tcl
       
       (image-vdi
        (list 'image (id win) (id item) (x-pos item) (y-pos item)
              (file item) (width item) (height item)))))))

