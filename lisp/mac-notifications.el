;;; mac-notify.el --- OSX Native Notifications       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Christian Hopps

;; Author: Christian Hopps <chopps@ja.int.chopps.org>
;; Keywords: calendar, desktop, mail, notifications
(require 'cl)

(unless (fboundp 'mac-notification-os-send)
  (error "mac notifications unsupported in this version of Emacs"))

(unless (boundp 'debug-mac-notifications)
  (defvar debug-mac-notifications nil))

(defvar mac-notification-action-hash (make-hash-table :test 'equal)
  "A hash table for looking up category names by action list

An action list has the form '(\"key1\" \"Title1\" ...)")
(puthash nil "generic" mac-notification-action-hash)

(defvar mac-notification-category-hash (make-hash-table :test 'equal)
  "A hash table for looking up action lists by category name.

An action list has the form '(\"key1\" \"Title1\" ...)")
(puthash "generic" nil mac-notification-category-hash)

(defvar mac-notification-id-hash (make-hash-table :test 'equal)
  "A hash table for looking up posted notifications.")

(defun mac-notification-add-category (category actions)
  "Add the CATEGORY for the list of actions ACTIONS

It is safe to call this multiple times for the same CATEGORY as
long as the same ACTIONS are passed each time.  An action list
has the form '((\"KEY\" [\"TITLE\" [OPTIONS]]) ...)

If KEY is used for TITLE if TITLE is not given.

OPTIONS specify options for the category which can be any number
of the following atoms:

  `authentication-required' - action can only be taken on unlocked
  device.

  `destructive' - the action will perform some destructive
  action (and is displayed differently).

  `foreground' - the action should cause Emacs to launch into the
  foreground.

Additionally in options you can include 2 properties to provide
for textual feedback from the user. This text is returned in the
`:user-text' property of the CONTENT (see the :on-action and
`:on-close' properties in `mac-notification-send' function.

:text-input-button TEXT -- When the action is taken by the user,
                           the user is presented with a text
                           input area labeled with TEXT to
                           provide text feedback.
:text-input-placeholder TEXT -- When the action is taken by the user,
                           the user is presented with a text
                           input area with sample TEXT in the
                           input area.

"
  (let ((existing-actions (gethash category mac-notification-category-hash)))
    (if existing-actions
        (if (not (equal actions existing-actions))
            (error (format "mac notification category '%s' already exists with different actions" category)))
      ;; Create a new category for the action list
      (mac-notification-os-add-category category actions)
      (unless (gethash actions mac-notification-action-hash)
        (puthash actions category mac-notification-action-hash))
      (puthash category actions mac-notification-category-hash))
    category))

(mac-notification-add-category "show" '(("show" "Show")))
(mac-notification-add-category "dismiss" '(("dismiss" "Dismiss")))

(defun mac-notification-send (title body &rest params)
  "Post a notification with the given TITLE and BODY.

Additional parameters for the notification given in PARAMS. An
IDENTIFIER uniquely representing this notification is returned.

One predefined category exists, \"generic\" with the following
properties:

\"generic\"  -- Displays the notification with no action buttons. Emacs
                will be activated/launched.

The following additional parameters are supported:

:badge NUMBER       -- 0 to clear otherwise set badge to this number
:category CATEGORY  -- category which specified the actions that will
                       be associated with this notification. Either
                       one of the predefined categories (default is
                       \"generic\") or ones defined by
                       `mac-notification-add-category`.
:on-action             -- Callback invoked when user performs action on
                          notification (on-action ID ACITON CONTENT)
:on-close              -- Callback invoked when notification is closed
                          (on-close ID REASON CONTENT)
:sound-name SOUND-NAME -- Play the system sound identified by SOUND-NAME.
:subtitle SUBTITLE     -- Display the given SUBTITLE under the TITLE.

:target-content STRING -- Returned to help identify the notification.
:thread-identifier STRING -- A value to group notifications under.

When the user responds to the notification, and if a :on-action callback
has been specified, it is called with 3 arguments: ID ACTION CONTENT.
ID is the identifier for the notification which can be used to get the
from the `mac-notification-id-hash'. ACTION is the KEY value for the
action as defined in the CATEGORY.

CONTENT is a plist which contains most of the values defined in
UNNotificationContent (e.g., the title can be found in `:title'
if non-nil) Additionally `:identifier' is set the the identifier
for this notification, `:category' is the category that defined
the actions. All values are strings except :badge which is a
number. If the CATEGORY defined user text input that will be
returned in `:user-text'. Any unrecognized properties passed in PARAMS
whose values are either symbolp or stringp will also be returned in
CONTENT (as strings). Below is a list of the properties that may be
present:

':badge'       -- as passed in
':body'        -- as passed in
':category'    -- as passed in
':identifier'  -- as returned on creation of notification
':subtitle'    -- as passed in
':title'       -- as passed in
':target-content'    -- as passed in (seems to go missing)
':thread-identifier' -- as passed in (seems to go missing)
':user-text'         -- if category defines text input option from the
                        user and it was given, this is that value.

When the notification is closed, and if a :on-close callback has
been specified, it is called with 3 arguments: ID REASON and
CONTENT.  ID and CONTENT are the same values as with :on-action, and
CLOSE-REASON indicates how the notification was closed and can be one of:

'close-notification -- The notification was closed normally indicating
                       that Emacs should act on it.
'dismissed          -- The notification was dismissed indicating
                       that Emacs should not act upon it.
'expired            -- The notification expired.
'undefined          -- unknown, treat as dismissed.
"
  (interactive "stitle: \nsbody: ")
  (let ((id (apply #'mac-notification-os-send title body params)))
    (puthash id (list title body params) mac-notification-id-hash)))

(defun mac-notification~unflatten (l)
  (if l (cons (list (car l) (cadr l)) (mac-notification~unflatten (cddr l)))
    l))

(defun mac-notification-get-category (actions)
  "Get the category for the action list ACTIONS, creating if necessary.

An action list has the form '(\"key1\" \"Title1\" ...)"
  (setq actions (mac-notification~unflatten actions))
  (let ((category (gethash actions mac-notification-action-hash)))
    (unless category
      ;; Create a new category for this unique action list
      (setq category (symbol-name (cl-gensym)))
      (mac-notification-os-add-category category actions)
      (puthash actions category mac-notification-action-hash)
      (puthash category actions mac-notification-category-hash))
    category))

(defun mac-notification-receive (action close-reason content)
  (let* ((id (plist-get content :identifier))
         (params (caddr (gethash id mac-notification-id-hash)))
         ;; prefer what we have locally for actions
         (on-action (or (plist-get params :on-action) (plist-get content :on-action)))
         (on-close (or (plist-get params :on-close) (plist-get content :on-close)))
         (on-action-compat (plist-get params :on-action-compat))
         (on-close-compat (plist-get params :on-close-compat)))
    ;; Convert string to symbol if need be
    (when (stringp on-action) (setq on-action (intern on-action)))
    (when (stringp on-close) (setq on-close (intern on-close)))
    (when (stringp on-action-compat) (setq on-action-compat (intern on-action-compat)))
    (when (stringp on-close-compat) (setq on-close-compat (intern on-close-compat)))
    (when (functionp on-action-compat)
      (funcall on-action-compat id action))
    (when (functionp on-action)
      (funcall on-action id action content))
    (when (functionp on-close-compat)
      (funcall on-close-compat id close-reason))
    (when (functionp on-close)
      (funcall on-close id close-reason content))
    (remhash id mac-notification-id-hash)))

;; We are going to override the generic notifications which actually
;; only use d-bus
(require 'notifications)

;; Override dbus-centric version
(defun notifications-notify (&rest params)
  (with-demoted-errors
      (let* ((actions (plist-get params :actions))
             (category (mac-notification-get-category actions))
             (params (plist-put params :category category))
             (on-action (plist-get params :on-action))
             (params (plist-put params :on-action-compat on-action))
             (params (plist-put params :on-action nil))
             (on-close (plist-get params :on-close))
             (params (plist-put params :on-close-compat on-close))
             (params (plist-put params :on-close nil))
	     (title (plist-get params :title))
	     (body (plist-get params :body))
             (id (apply #'mac-notification-os-send title body params)))
        (puthash id (list title body params) mac-notification-id-hash))))

(provide 'mac-notifications)
