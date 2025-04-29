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

(define-private (validate-property-tag (tag (string-ascii 30)))
    (and
        (> (len tag) u0)
        (<= (len tag) u30)
    )
)

(define-private (validate-description-text (description (string-ascii 200)))
    (and
        (>= (len description) u1)
        (<= (len description) u200)
    )
)

(define-private (validate-classification-tag (classification (string-ascii 20)))
    (and
        (>= (len classification) u1)
        (<= (len classification) u20)
    )
)

(define-private (validate-permission-tier (permission-tier (string-ascii 10)))
    (or
        (is-eq permission-tier PERMISSION_VIEW)
        (is-eq permission-tier PERMISSION_EDIT)
        (is-eq permission-tier PERMISSION_OVERSEE)
    )
)

(define-private (validate-time-frame (duration uint))
    (and
        (> duration u0)
        (<= duration u52560) ;; One year maximum in block terms
    )
)

(define-private (validate-collaborator (collaborator principal))
    (not (is-eq collaborator tx-sender))
)

(define-private (is-archive-custodian (archive-id uint) (user principal))
    (match (map-get? digital-archives { archive-id: archive-id })
        entry (is-eq (get custodian entry) user)
        false
    )
)

(define-private (archive-exists (archive-id uint))
    (is-some (map-get? digital-archives { archive-id: archive-id }))
)

(define-private (validate-edit-permission (can-edit bool))
    (or (is-eq can-edit true) (is-eq can-edit false))
)

;; Core System Functions
(define-public (create-archive 
    (document-name (string-ascii 50))
    (integrity-signature (string-ascii 64))
    (description-text (string-ascii 200))
    (classification-tag (string-ascii 20))
    (property-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (new-id (+ (var-get archive-counter) u1))
            (current-block block-height)
        )
        ;; Input validation
        (asserts! (validate-document-name document-name) ERR_MALFORMED_DATA)
        (asserts! (validate-integrity-signature integrity-signature) ERR_MALFORMED_DATA)
        (asserts! (validate-description-text description-text) ERR_INVALID_DESCRIPTORS)
        (asserts! (validate-classification-tag classification-tag) ERR_INVALID_CLASSIFICATION)
        (asserts! (validate-property-tags property-tags) ERR_INVALID_DESCRIPTORS)

        ;; Create archive entry
        (map-set digital-archives
            { archive-id: new-id }
            {
                document-name: document-name,
                custodian: tx-sender,
                integrity-signature: integrity-signature,
                description-text: description-text,
                timestamp-created: current-block,
                timestamp-updated: current-block,
                classification-tag: classification-tag,
                property-tags: property-tags
            }
        )

        ;; Update counter
        (var-set archive-counter new-id)
        (ok new-id)
    )
)

(define-public (modify-archive
    (archive-id uint)
    (new-document-name (string-ascii 50))
    (new-integrity-signature (string-ascii 64))
    (new-description-text (string-ascii 200))
    (new-property-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (archive-entry (unwrap! (map-get? digital-archives { archive-id: archive-id }) ERR_ARCHIVE_MISSING))
        )
        ;; Verify authorization
        (asserts! (is-archive-custodian archive-id tx-sender) ERR_ACCESS_FORBIDDEN)

        ;; Validate inputs
        (asserts! (validate-document-name new-document-name) ERR_MALFORMED_DATA)
        (asserts! (validate-integrity-signature new-integrity-signature) ERR_MALFORMED_DATA)
        (asserts! (validate-description-text new-description-text) ERR_INVALID_DESCRIPTORS)
        (asserts! (validate-property-tags new-property-tags) ERR_INVALID_DESCRIPTORS)

        ;; Update archive entry
        (map-set digital-archives
            { archive-id: archive-id }
            (merge archive-entry {
                document-name: new-document-name,
                integrity-signature: new-integrity-signature,
                description-text: new-description-text,
                timestamp-updated: block-height,
                property-tags: new-property-tags
            })
        )
        (ok true)
    )
)

(define-public (establish-collaboration
    (archive-id uint)
    (collaborator principal)
    (permission-tier (string-ascii 10))
    (time-frame uint)
    (editing-allowed bool)
)
    (let
        (
            (current-block block-height)
            (expiration-block (+ current-block time-frame))
        )
        ;; Validate inputs and permissions
        (asserts! (archive-exists archive-id) ERR_ARCHIVE_MISSING)
        (asserts! (is-archive-custodian archive-id tx-sender) ERR_ACCESS_FORBIDDEN)
        (asserts! (validate-collaborator collaborator) ERR_MALFORMED_DATA)
        (asserts! (validate-permission-tier permission-tier) ERR_INVALID_PERMISSION_TYPE)
        (asserts! (validate-time-frame time-frame) ERR_INVALID_TIME_FRAME)
        (asserts! (validate-edit-permission editing-allowed) ERR_MALFORMED_DATA)

        ;; Create collaboration record
        (map-set collaboration-permissions
            { archive-id: archive-id, collaborator: collaborator }
            {
                permission-tier: permission-tier,
                timestamp-granted: current-block,
                timestamp-expiration: expiration-block,
                editing-allowed: editing-allowed
            }
        )
        (ok true)
    )
)

;; Extended System Functionality
;; Streamlined modification process with improved structure
(define-public (enhanced-archive-update
    (archive-id uint)
    (new-document-name (string-ascii 50))
    (new-integrity-signature (string-ascii 64))
    (new-description-text (string-ascii 200))
    (new-property-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (archive-entry (unwrap! (map-get? digital-archives { archive-id: archive-id }) ERR_ARCHIVE_MISSING))
        )
        ;; Verify ownership
        (asserts! (is-archive-custodian archive-id tx-sender) ERR_ACCESS_FORBIDDEN)

        ;; Construct updated entry
        (let
            (
                (updated-entry (merge archive-entry {
                    document-name: new-document-name,
                    integrity-signature: new-integrity-signature,
                    description-text: new-description-text,
                    property-tags: new-property-tags
                }))
            )
            ;; Apply updates
            (map-set digital-archives { archive-id: archive-id } updated-entry)
            (ok true)
        )
    )
)


;; Security-enhanced archive modification with comprehensive validation
(define-public (secured-archive-modification
    (archive-id uint)
    (new-document-name (string-ascii 50))
    (new-integrity-signature (string-ascii 64))
    (new-description-text (string-ascii 200))
    (new-property-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (archive-entry (unwrap! (map-get? digital-archives { archive-id: archive-id }) ERR_ARCHIVE_MISSING))
            (current-custodian (get custodian archive-entry))
        )
        ;; Multiple security validations
        (asserts! (is-eq current-custodian tx-sender) ERR_ACCESS_FORBIDDEN)
        (asserts! (is-archive-custodian archive-id tx-sender) ERR_ACCESS_FORBIDDEN)

        ;; Comprehensive data validation
        (asserts! (validate-document-name new-document-name) ERR_MALFORMED_DATA)
        (asserts! (validate-integrity-signature new-integrity-signature) ERR_MALFORMED_DATA)
        (asserts! (validate-description-text new-description-text) ERR_INVALID_DESCRIPTORS)
        (asserts! (validate-property-tags new-property-tags) ERR_INVALID_DESCRIPTORS)

        ;; Apply changes with timestamp update
        (map-set digital-archives
            { archive-id: archive-id }
            (merge archive-entry {
                document-name: new-document-name,
                integrity-signature: new-integrity-signature,
                description-text: new-description-text,
                timestamp-updated: block-height,
                property-tags: new-property-tags
            })
        )
        (ok true)
    )
)

;; Alternative data structure for specialized query patterns
(define-map indexed-digital-archives
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

(define-public (indexed-archive-creation
    (document-name (string-ascii 50))
    (integrity-signature (string-ascii 64))
    (description-text (string-ascii 200))
    (classification-tag (string-ascii 20))
    (property-tags (list 5 (string-ascii 30)))
)
    (let
        (
            (new-id (+ (var-get archive-counter) u1))
            (current-block block-height)
            (custodian-principal tx-sender)
        )
        ;; Sequential validation for clarity
        (asserts! (validate-document-name document-name) ERR_MALFORMED_DATA)
        (asserts! (validate-integrity-signature integrity-signature) ERR_MALFORMED_DATA)
        (asserts! (validate-description-text description-text) ERR_INVALID_DESCRIPTORS)
        (asserts! (validate-classification-tag classification-tag) ERR_INVALID_CLASSIFICATION)
        (asserts! (validate-property-tags property-tags) ERR_INVALID_DESCRIPTORS)

        ;; Initialize in both data structures for different query patterns
        (map-set indexed-digital-archives
            { archive-id: new-id }
            {
                document-name: document-name,
                custodian: custodian-principal,
                integrity-signature: integrity-signature,
                description-text: description-text,
                timestamp-created: current-block,
                timestamp-updated: current-block,
                classification-tag: classification-tag,
                property-tags: property-tags
            }
        )

        ;; Update counter and return new ID
        (var-set archive-counter new-id)
        (ok new-id)
    )
)

