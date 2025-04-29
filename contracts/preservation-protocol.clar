;; Digital Information Preservation System
;; A comprehensive solution for secure storage and controlled access to 
;; digital archives and important information

;; Error Constants
(define-constant ERR_ACCESS_FORBIDDEN (err u100))
(define-constant ERR_MALFORMED_DATA (err u101))
(define-constant ERR_ARCHIVE_MISSING (err u102))
(define-constant ERR_DUPLICATE_ARCHIVE (err u103))
(define-constant ERR_INVALID_DESCRIPTORS (err u104))
(define-constant ERR_NOT_AUTHORIZED (err u105))
(define-constant ERR_INVALID_TIME_FRAME (err u106))
(define-constant ERR_INVALID_PERMISSION_TYPE (err u107))
(define-constant ERR_INVALID_CLASSIFICATION (err u108))
(define-constant PLATFORM_ADMINISTRATOR tx-sender)

;; Permission Tier Constants
(define-constant PERMISSION_VIEW "read")
(define-constant PERMISSION_EDIT "write")
(define-constant PERMISSION_OVERSEE "admin")

;; State Management 
(define-data-var archive-counter uint u0)

;; Primary Data Structures
(define-map digital-archives
    { archive-id: uint }
    {
        document-name: (string-ascii 50),
        custodian: principal,
        integrity-signature: (string-ascii 64),
        description-text: (string-ascii 200),
        timestamp-created: uint,
        timestamp-updated: uint,
        classification-tag: (string-ascii 20),
        property-tags: (list 5 (string-ascii 30))
    }
)

(define-map collaboration-permissions
    { archive-id: uint, collaborator: principal }
    {
        permission-tier: (string-ascii 10),
        timestamp-granted: uint,
        timestamp-expiration: uint,
        editing-allowed: bool
    }
)

;; Validation Functions
(define-private (validate-document-name (name (string-ascii 50)))
    (and
        (> (len name) u0)
        (<= (len name) u50)
    )
)

(define-private (validate-integrity-signature (signature (string-ascii 64)))
    (and
        (is-eq (len signature) u64)
        (> (len signature) u0)
    )
)

(define-private (validate-property-tags (tags (list 5 (string-ascii 30))))
    (and
        (>= (len tags) u1)
        (<= (len tags) u5)
        (is-eq (len (filter validate-property-tag tags)) (len tags))
    )
)
