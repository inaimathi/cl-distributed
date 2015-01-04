(in-package #:cl-distributed)

(defparameter *arc* (mk-archive ""))

(define-handler (document/source :close-socket? nil) ()
  (subscribe! :updates sock)
  :ok)

(define-json-handler (document/show) ()
  `((:id . ,(current-id *arc*))
    (:body . ,(current *arc*))
    (:user . ,(gensym))))

(defun json-plist (&rest plist) (cl-json:encode-json-plist-to-string plist))

(define-json-handler (document/insert) ((user :keyword) (id :integer (>= id 0)) (ix :integer (>= ix 0)) (text :string (> (length text) 0)))
  (update! *arc* id (mk-insertion ix text))
  (publish! :updates (make-sse (json-plist :user user :id (current-id *arc*) :ix ix :text text) :event "insert"))
  (current-id *arc*))

(define-json-handler (document/delete) ((user :keyword) (id :integer (>= id 0)) (ix :integer (>= ix 0)) (ct :integer (> ct 0)))
  (update! *arc* id (mk-deletion ix ct))
  (publish! :updates (make-sse (json-plist :user user :id (current-id *arc*) :ix ix :ct ct)
			       :event "delete"))
  (current-id *arc*))

;;;;;;;;;; Front-end
(define-handler (js/distributed.js :content-type "text/javascript") ()
  (ps (defvar *editor* nil)
      (defvar *doc* (create :my-edits (create)))
      (defvar *my-id* nil)
      
      (defun store-edit! (id edit)
	(when (> (@ *doc* id) id)
	  (setf (aref *doc* :my-edits id) edit)))
      
      (defun sync-id! (id)
	(setf (@ *doc* id) id)
	(delete (aref *doc* :my-edits id)))

      (defun doc/delete! (ix ct)
	(post/json
	 "/document/delete" (create :user *my-id* :id (@ *doc* id) :ix ix :ct ct)
	 (lambda (res)
	   (console.log "NEW ID" res)
	   (store-edit! res (create :type :delete :id res :ix ix :ct ct)))))

      (defun doc/insert! (ix text)
	(post/json 
	 "/document/insert" (create :user *my-id* :id (@ *doc* id) :ix ix :text text)
	 (lambda (res)
	   (console.log "NEW ID" res)
	   (store-edit! res (create :type :insert :id res :ix ix :text text)))))

      (defun doc-events (editor)
	(json-event-source
	 "/document/source"
	 (create
	  "insert" (lambda (res)
		     (let ((id (@ res id)))
		       (when (> id (@ *doc* id))
			 (console.log "New id" id res)
			 (unless (equal (@ res user) *my-id*)
			   (chain editor (replace-range (@ res text) (create :line 0 :ch (@ res ix))
							nil "server-synch")))
			 (sync-id! id))))
	  "delete" (lambda (res id)
		     (let ((id (@ res id)))
		       (when (> id (@ *doc* id))
			 (console.log "New id" id res)
			 (unless (equal (@ res user) *my-id*)
			   (chain editor (replace-range "" (create :line 0 :ch (@ res ix)) 
							(create :line 0 :ch (+ (@ res ix) (@ res ct)))
							"server-synch")))
			 (sync-id! id)))))))

      (dom-ready 
       (lambda ()
	 (console.log "Testing...")
	 (post/json 
	  "/document/show" (create)
	  (lambda (res)
	    (let* ((textarea (by-selector "#editor"))
		   (editor (chain -code-mirror (from-text-area textarea))))
	      (setf *editor* editor
		    (@ *doc* id) (@ res id)
		    (@ *doc* body) (@ res body)
		    *my-id* (@ res user))
	      (doc-events editor)
	      (chain editor (set-value (@ res body)))
	      
	      (chain 
	       editor 
	       (on 'change
		   (lambda (mirror change)
		     (case (@ change origin)
		       ("+delete"
			;; (console.log "DELETION" change)
			(doc/delete! (@ change from ch) (length (@ change removed 0))))
		       ("+input"
			;; (console.log "INSERTION" change)
			(doc/insert! (@ change from ch) (@ change text 0)))
		       ("server-synch" nil)
		       (t (console.log "UNSUPPORTED CHANGE" change)))))))))))))

(define-handler (root) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:title "distributED")
      
      (:link :rel "stylesheet" :href "/static/css/genericons.css")
      (:link :rel "stylesheet" :href "/static/css/codemirror.css")
      (:link :rel "stylesheet" :href "/static/css/dialog.css")
      (:link :rel "stylesheet" :href "/static/css/show-hint.css")

      (:script :type "text/javascript" :src "/static/js/Blob.js")
      (:script :type "text/javascript" :src "/static/js/FileSaver.js")

      (:script :type "text/javascript" :src "/static/js/codemirror.js")
      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/distributed.js")
      
      (:script :type "text/javascript" :src "/static/js/modes/commonlisp.js")
      (:script :type "text/javascript" :src "/static/js/addons/comment.js")
      (:script :type "text/javascript" :src "/static/js/addons/closebrackets.js")
      (:script :type "text/javascript" :src "/static/js/addons/matchbrackets.js")
      (:script :type "text/javascript" :src "/static/js/addons/search.js")
      (:script :type "text/javascript" :src "/static/js/addons/searchcursor.js")
      (:script :type "text/javascript" :src "/static/js/addons/match-highlighter.js")
      (:script :type "text/javascript" :src "/static/js/addons/active-line.js")
      (:script :type "text/javascript" :src "/static/js/addons/mark-selection.js")
      (:script :type "text/javascript" :src "/static/js/addons/show-hint.js")
      (:script :type "text/javascript" :src "/static/js/addons/anyword-hint.js")
      (:script :type "text/javascript" :src "/static/js/addons/dialog.js")
      (:script :type "text/javascript" :src "/static/js/addons/runmode/runmode.js"))

     (:body
      (:textarea :id "editor" :style "width: 90%; height: 560px;")))))

(define-handler (js/base.js :content-type "application/javascript") ()
  (ps 
    ;;;; base.js contains general utilities that might be useful in other JS
    ;;;; applications too. Nothing notebook-specific here.

    ;; basic functional stuff
    (defun identity (thing) thing)

    (defun constantly (thing) (lambda () thing))

    (defun rest (array) (chain array (slice 1)))

    (defun map (fn thing)
      (if (object? thing)
	  (let ((res (-array)))
	    (for-in (k thing) (chain res (push (fn (aref thing k) k))))
	    res)
	  (loop for elem in thing collect (fn elem))))

    (defun fold (fn memo thing)
      (let ((m memo))
	(if (object? thing)
	    (for-in (k thing) (setf m (fn (aref thing k) m)))
	    (loop for elem in thing do (setf m (fn elem m))))
	m))

    (defun filter (fn thing)	
      (loop for elem in thing when (fn elem) collect elem))

    (defun extend (obj &rest other-objs)
      (flet ((ext! (dest src) (map (lambda (v k) (setf (aref dest k) v)) src)))
	(let ((res (create)))
	  (ext! res obj)
	  (map (lambda (obj) (ext! res obj)) other-objs)
	  res)))

    (defun append-new (list-a list-b)
      (let ((s (new (-set list-a)))
	    (lst (map #'identity list-a)))
	(loop for elem in list-b
	   unless (chain s (has elem))
	   do (chain lst (push elem)))
	lst))

    (defun lines (string) (chain string (split #\newline)))

    (defun join (strings &optional (separator "")) (chain strings (join separator)))

    ;; basic hash/array stuff
    (defun vals (obj) (map identity obj))
    (defun keys (obj) (map (lambda (v k) k) obj))
    (defun last (array) (aref array (- (length array) 1)))

    (defun member? (elem thing)
      (if (object? thing)
	  (in elem thing)
	  (chain thing (index-of elem))))

    (defun equal? (a b)
      (let ((type-a (typeof a))
	    (type-b (typeof b)))
	(and (equal type-a type-b)
	     (cond
	       ((member? type-a (list "number" "string" "function"))
		(equal a b))
	       ((array? a)
		(and
		 (= (length a) (length b))
		 (loop for elem-a in a for elem-b in b
		    unless (equal? elem-a elem-b) return f
		    finally (return t))))
	       ((object? a)
		;; object comparison here
		;; and 
		;;   all keys of a are in b
		;;   all keys of b are in a
		;;   all keys of a and b have the same values
		nil
		)))))

    ;; basic regex stuff
    (defun regex-match (regex string)
      (chain (-reg-exp regex) (test string)))
    (defun regex-match-any (string &rest regexes)
      (loop for reg in regexes
	 when (regex-match reg string) return t
	 finally (return nil)))

    (defun matching? (regex)
      (lambda (string) (regex-match regex string)))

    ;; basic DOM/event stuff
    (defun sheet-text (sheet &optional (predicate identity))
      (if (number? sheet)
	  (sheet-text (aref (@ document style-sheets) sheet) predicate)
	  (join
	   (loop for rule in (@ sheet css-rules)
	      for text = (@ rule css-text)
	      when (predicate text) collect text)	   
	   #\newline)))
    
    (defun dom-ready (callback)
      (chain document (add-event-listener "DOMContentLoaded" callback)))

    (defun remove-all-event-handlers (elem)
      (let ((clone (chain elem (clone-node t))))
	(chain elem parent-node (replace-child clone elem))
	clone))

    (defun prevent (ev) (when ev (chain ev (prevent-default))))

    (defun debounce (fn delay immediate?)
      (let ((timeout))
	(lambda ()
	  (let ((context this)
		(args arguments))
	    (clear-timeout timeout)
	    (setf timeout (set-timeout 
			   (lambda ()
			     (setf timeout nil)
			     (unless immediate?
			       (chain fn (apply context args))))
			   delay))
	    (when (and immediate? (not timeout))
	      (chain fn (apply context args)))))))

    (defun scroll-to-elem (elem)
      (let ((x (@ elem offset-left))
	    (y (@ elem offset-top)))
	(chain window (scroll-to x y))))

    (defun show! (elem)
      (setf (@ elem hidden) nil))
    (defun hide! (elem)
      (setf (@ elem hidden) t))

    (defun by-selector (selector)
      (chain document (query-selector selector)))
    (defun by-selector-all (selector)
      (chain document (query-selector-all selector)))

    (defun dom-escape (string)
      (when (string? string)
	(chain string
	       (replace "<" "&lt;")
	       (replace ">" "&gt;"))))

    (defun dom-append (elem markup)
      (let ((new-content (chain document (create-element "span"))))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop while (@ new-content first-child)
	   do (chain elem (append-child (@ new-content first-child))))))

    (defun dom-replace (elem markup)
      (let ((new-content (chain document (create-element "span")))
	    (parent (@ elem parent-node)))
	(setf (@ new-content inner-h-t-m-l) markup)
	(loop for child in (@ new-content child-nodes)
	   do (chain parent (insert-before new-content elem)))
	(chain elem (remove))))

    (defun dom-set (elem markup)
      (setf (@ elem inner-h-t-m-l) markup))

    ;; basic type stuff
    (defun number? (obj) (string= "number" (typeof obj)))
    (defun string? (obj) (string= "string" (typeof obj)))
    (defun function? (obj) (string= "function" (typeof obj)))

    (defun type? (obj type-string)
      (eql (chain -object prototype to-string (call obj)) type-string))
    (defun array? (arr) (type? arr "[object Array]"))
    (defun object? (obj) (type? obj "[object Object]"))

    ;; basic encoding/decoding stuff
    (defun encode (string)
      (encode-u-r-i-component string))

    (defun decode (string)
      (decode-u-r-i string))
    
    (defun string->obj (string)
      (chain -j-s-o-n (parse string)))

    (defun obj->string (object)
      (chain -j-s-o-n (stringify object)))

    (defun obj->params (object)
      (join 
       (map (lambda (v k) 
	      (+ (encode k) "=" 
		 (encode (if (object? v) (obj->string v) v))))
	    object)
       "&"))

    ;; basic AJAX stuff
    (defun save-file (filename contents &optional (type "application/json;charset=utf-8"))
      (let* ((content-string (if (string? contents) contents (obj->string contents)))
	     (blob (new (-blob (list content-string) (create :type type)))))
	(save-as blob filename)))

    (defun get-page-hash ()
      (let ((hash (@ window location hash))
	    (res (create)))
	(when hash
	  (loop for pair in (chain (rest hash) (split "&"))
	     for (k v) = (chain pair (split "="))
	     do (setf (aref res (decode k)) (decode v)))
	  res)))
    
    (defun set-page-hash (hash-object)
      (setf (@ window location hash) (obj->params hash-object)))

    (defun get (uri params callback)
      (let ((req (new (-x-m-l-http-request))))
	(setf (@ req onreadystatechange)
	      (lambda ()
		(when (and (equal (@ req ready-state) 4)
			   (equal (@ req status) 200))
		  (let ((result (@ req response-text)))
		    (callback result)))))
	(chain req (open :GET (if params (+ uri "?" (obj->params params)) uri) t))
	(chain req (send))))

    (defun post (uri params on-success on-fail)
      (let ((req (new (-x-m-l-http-request)))
	    (encoded-params (obj->params params)))
	(setf (@ req onreadystatechange)
	      (lambda ()
		(when (equal (@ req ready-state) 4)
		  (if (equal (@ req status) 200)
		      (when (function? on-success)
			(let ((result (@ req response-text)))
			  (on-success result)))
		      (when (function? on-fail)
			(on-fail req))))))
	(chain req (open :POST uri t))
	(chain req (set-request-header "Content-type" "application/x-www-form-urlencoded"))
	(chain req (set-request-header "Content-length" (length encoded-params)))
	(chain req (set-request-header "Connection" "close"))
	(chain req (send encoded-params))))

    (defun post/json (uri params on-success on-fail)
      (post uri params
	    (lambda (raw)
	      (when (function? on-success)
		(let ((res (string->obj raw)))
		  (on-success res))))
	    on-fail))

    (defun event-source (uri bindings)
      (let ((stream (new (-event-source uri))))
	(setf (@ stream onopen) (lambda (e) (console.log "Stream OPENED!"))
	      (@ stream onerror) (lambda (e) (console.log "Stream ERRORED!"))
	      (@ stream onmessage) (lambda (e) (console.log "GOT UNMARKED MESSAGE" a)))
	(map (lambda (v k) (chain stream (add-event-listener k v)))
	     bindings)
	stream))
    
    (defun json-event-source (uri bindings)
      (let ((stream (new (-event-source uri))))
	(setf (@ stream onopen) (lambda (e) (console.log "Stream OPENED!"))
	      (@ stream onerror) (lambda (e) (console.log "Stream ERRORED!"))
	      (@ stream onmessage) (lambda (e) (console.log "GOT UNMARKED MESSAGE" a)))
	(map (lambda (v k) 
	       (chain stream (add-event-listener 
			      k (lambda (res) (v (string->obj (@ res data)) (@ res id))))))
	     bindings)
	stream))))

;;;;;;;;;; Static
(define-file-handler "/home/inaimathi/quicklisp/local-projects/cl-distributed/static/" :stem-from "static")

(start 4343)
