(in-package #:cl-distributed)

(defparameter *arc* (mk-archive "" :file "test.arc"))

(define-handler (document/source :close-socket? nil) ()
  (subscribe! :updates sock)
  :ok)

(define-json-handler (document/show) ()
  (unless (lookup :user session)
    (setf (lookup :user session) (gensym)))
  `((:id . ,(current-id *arc*))
    (:body . ,(current *arc*))
    (:user . ,(lookup :user session))))

(defun json-plist (&rest plist)
  (cl-json:encode-json-plist-to-string plist))

(define-json-handler (document/insert) ((id :integer (>= id 0)) (ix :integer (>= ix 0)) (text :string (> (length text) 0)))
  (let ((recd (update! *arc* id (mk-insertion ix text))))
    (publish! 
     :updates 
     (make-sse
      (json-plist 
       :user (lookup :user session)
       :id (current-id *arc*)
       :ix (ix recd) :text (text recd))
      :event "insert")))
  (current-id *arc*))

(define-json-handler (document/delete) ((id :integer (>= id 0)) (ix :integer (>= ix 0)) (ct :integer (> ct 0)))
  (let ((recd (update! *arc* id (mk-deletion ix ct))))
    (publish! 
     :updates 
     (make-sse 
      (json-plist 
       :user (lookup :user session) 
       :id (current-id *arc*) 
       :ix (ix recd) :ct (ct recd))
      :event "delete")))
  (current-id *arc*))

;;;;;;;;;; Front-end
(define-handler (js/distributed.js :content-type "text/javascript") ()
  (ps (defvar *editor* nil)
      (defvar *doc* (create :my-edits (new (-array))))
      (defvar *my-id* nil)
      
      (defun store-edit! (id edit)
	(when (> (@ *doc* id) id)
	  (chain (@ *doc* :my-edits) (push edit))))
      
      (defun sync-id! (id)
	(setf (@ *doc* id) id))

      (defun filter-locals! (id)
	(setf (@ *doc* :my-edits) 
	      (filter (lambda (ed) (> (@ ed id) id)) (@ *doc* :my-edits))))

      (defun doc/delete! (ix text)
	(post/json
	 "/document/delete" (create :id (@ *doc* id) :ix ix :ct (length text))
	 (lambda (res)
	   (console.log "NEW ID" res)
	   (store-edit! res (create :type :delete :id res :ix ix :ct (length text) :text text)))))

      (defun doc/insert! (ix text)
	(post/json 
	 "/document/insert" (create :id (@ *doc* id) :ix ix :text text)
	 (lambda (res)
	   (console.log "NEW ID" res)
	   (store-edit! res (create :type :insert :id res :ix ix :text text)))))

      (defun ev (fn)
	(lambda (res)
	  (chain *editor*
		 (operation 
		  (lambda ()
		    (let ((id (@ res id))
			  (cur (chain *editor* (get-cursor))))
		      (dom-set (by-selector "#cur-id") id)
		      (dom-append 
		       (by-selector "ul#events")
		       (who-ps-html (:li (obj->string res))))
		      (when (> id (@ *doc* id))
			(unless (equal (@ res user) *my-id*)
			  (fn res))
			(chain *editor* (set-cursor cur))
			(sync-id! id))))))))
      
      (defun apply-single-edit (body ed)
	(if (eq :insert (@ ed type))
	    (+ (chain body (susbstr 0 (@ ed ix)))
	       (@ ed text)
	       (chain body (substr (@ ed ix))))
	    (+ (chain body (substr 0 (@ ed ix)))
	       (chain val (substr (+ (@ ed ix) (@ ed ct)))))))

      (defun reverse-single-edit (body ed)
	(apply-single-edit 
	 body (if (eq :insert (@ ed type))
		  (create :type :delete :ix (@ ed ix) :ct (length (@ ed text)))
		  (create :type :insert :ix (@ ed ix) :text (@ ed text)))))

      (defun reverse-local-edits (body)
	(fold (lambda (ed memo) (reverse-single-edit memo v))
	      body (reversed (@ *doc* :my-edits))))

      (defun reapply-local-edits (body)
	(fold (lambda (ed memo)
		(apply-single-edit memo v))
	      body (@ *doc* :my-edits)))

      (defun doc-events (editor)
	(json-event-source
	 "/document/source"
	 (create
	  "insert" (ev 
		    (lambda (res)
		      (let* ((val (reverse-local-edits (chain editor (get-value))))
			     (mod (+ (chain val (substr 0 (@ res ix)))
				     (@ res text)
				     (chain val (substr (@ res ix))))))
			(filter-locals! (@ res id))
			(let ((new-val (reapply-local-edits mod)))
			  (chain editor (set-value new-val))))))
	  "delete" (ev
		    (lambda (res)
		      (let* ((val (chain editor (get-value)))
			     (mod (+ (chain val (substr 0 (@ res ix)))
				     (chain val (substr (+ (@ res ix) (@ res ct)))))))
			(filter-locals! (@ res id))
			(let ((new-val (reapply-local-edits mod)))
			  (chain editor (set-value new-val)))))))))

      (defun cm-ix (ed from)
	(length (chain ed (get-range (create :ch 0 :line 0) from))))

      (dom-ready 
       (lambda ()
	 (post/json 
	  "/document/show" (create)
	  (lambda (res)
	    (let* ((textarea (by-selector "#editor"))
		   (editor (chain -code-mirror (from-text-area textarea))))
	      (dom-set (by-selector "#cur-id") (@ res id))
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
		     (let ((ix (length (chain editor (get-range (create :ch 0 :line 0) (@ change from))))))
		       (case (@ change origin)
			 ("+delete"
			  (doc/delete! ix (join (@ change removed) #\newline)))
			 ("+input"
			  (doc/insert! ix (join (@ change text) #\newline)))
			 ("cut"
			  (doc/delete! ix (length (join (@ change removed) #\newline))))
			 ("paste"
			  (doc/insert! ix (join (@ change text) #\newline)))
			 ("server-synch" nil)
			 ("setValue" nil)
			 (t (console.log "UNSUPPORTED CHANGE" change))))))))))))))

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
      (:textarea :id "editor" :style "width: 90%; height: 560px;")
      (:p "Current: " (:span :id "cur-id" 0))
      (:ul :id "events")))))

(define-handler (js/base.js :content-type "application/javascript") ()
  (ps 
    ;;;; base.js contains general utilities that might be useful in other JS
    ;;;; applications too. Nothing notebook-specific here.

    ;; basic functional stuff
    (defun identity (thing) thing)

    (defun constantly (thing) (lambda () thing))

    (defun rest (array) (chain array (slice 1)))

    (defun reversed (array)
      (loop for i from (- (length array) 1) downto 0
	 collect (aref array i)))

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

;; (start 4343)

