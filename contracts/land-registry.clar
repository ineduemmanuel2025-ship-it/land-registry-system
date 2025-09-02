
;; Land Registry System
;; A public land ownership database with boundary verification and development approval tracking

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_LAND_NOT_FOUND (err u101))
(define-constant ERR_LAND_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_COORDINATES (err u103))
(define-constant ERR_PENDING_APPROVAL (err u104))
(define-constant ERR_INVALID_STATUS (err u105))

;; Data Variables
(define-data-var land-id-nonce uint u0)
(define-data-var admin principal CONTRACT_OWNER)

;; Data Maps
(define-map LandRecords
    uint
    {
        owner: principal,
        coordinates: {x1: uint, y1: uint, x2: uint, y2: uint},
        area: uint,
        status: (string-ascii 20),
        registered-at: uint,
        last-updated: uint,
        development-approved: bool
    }
)

(define-map OwnershipHistory
    {land-id: uint, block-height: uint}
    {previous-owner: principal, new-owner: principal, transfer-date: uint}
)

(define-map BoundaryVerifications
    uint
    {verified: bool, verified-by: principal, verification-date: uint}
)

(define-map DevelopmentApprovals
    uint
    {
        approved: bool,
        approved-by: principal,
        approval-date: uint,
        development-type: (string-ascii 50),
        expiry-date: uint
    }
)

;; Public Functions

;; Register new land parcel
(define-public (register-land (coordinates {x1: uint, y1: uint, x2: uint, y2: uint}) (area uint))
    (let (
        (new-land-id (+ (var-get land-id-nonce) u1))
        (current-block-height stacks-block-height)
    )
        (asserts! (and 
            (> (get x2 coordinates) (get x1 coordinates))
            (> (get y2 coordinates) (get y1 coordinates))
            (> area u0)
        ) ERR_INVALID_COORDINATES)
        
        (map-set LandRecords new-land-id {
            owner: tx-sender,
            coordinates: coordinates,
            area: area,
            status: "registered",
            registered-at: current-block-height,
            last-updated: current-block-height,
            development-approved: false
        })
        
        (map-set OwnershipHistory 
            {land-id: new-land-id, block-height: current-block-height}
            {previous-owner: tx-sender, new-owner: tx-sender, transfer-date: current-block-height}
        )
        
        (var-set land-id-nonce new-land-id)
        (ok new-land-id)
    )
)

;; Transfer land ownership
(define-public (transfer-land (land-id uint) (new-owner principal))
    (let (
        (land-info (unwrap! (map-get? LandRecords land-id) ERR_LAND_NOT_FOUND))
        (current-block-height stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get owner land-info)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status land-info) "registered") ERR_INVALID_STATUS)
        
        (map-set LandRecords land-id 
            (merge land-info {
                owner: new-owner,
                last-updated: current-block-height
            })
        )
        
        (map-set OwnershipHistory 
            {land-id: land-id, block-height: current-block-height}
            {previous-owner: tx-sender, new-owner: new-owner, transfer-date: current-block-height}
        )
        
        (ok true)
    )
)

;; Verify land boundaries (admin only)
(define-public (verify-boundaries (land-id uint))
    (let (
        (land-info (unwrap! (map-get? LandRecords land-id) ERR_LAND_NOT_FOUND))
        (current-block-height stacks-block-height)
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
        
        (map-set BoundaryVerifications land-id {
            verified: true,
            verified-by: tx-sender,
            verification-date: current-block-height
        })
        
        (ok true)
    )
)

;; Approve development (admin only)
(define-public (approve-development (land-id uint) (development-type (string-ascii 50)) (validity-period uint))
    (let (
        (land-info (unwrap! (map-get? LandRecords land-id) ERR_LAND_NOT_FOUND))
        (current-block-height stacks-block-height)
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
        
        (map-set DevelopmentApprovals land-id {
            approved: true,
            approved-by: tx-sender,
            approval-date: current-block-height,
            development-type: development-type,
            expiry-date: (+ current-block-height validity-period)
        })
        
        (map-set LandRecords land-id 
            (merge land-info {
                development-approved: true,
                last-updated: current-block-height
            })
        )
        
        (ok true)
    )
)

;; Update land status (admin only)
(define-public (update-land-status (land-id uint) (new-status (string-ascii 20)))
    (let (
        (land-info (unwrap! (map-get? LandRecords land-id) ERR_LAND_NOT_FOUND))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
        
        (map-set LandRecords land-id 
            (merge land-info {
                status: new-status,
                last-updated: stacks-block-height
            })
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get land information
(define-read-only (get-land-info (land-id uint))
    (map-get? LandRecords land-id)
)

;; Get boundary verification status
(define-read-only (get-boundary-verification (land-id uint))
    (map-get? BoundaryVerifications land-id)
)

;; Get development approval status
(define-read-only (get-development-approval (land-id uint))
    (map-get? DevelopmentApprovals land-id)
)

;; Get ownership history
(define-read-only (get-ownership-history (land-id uint) (target-block-height uint))
    (map-get? OwnershipHistory {land-id: land-id, block-height: target-block-height})
)

;; Check if land is owned by address
(define-read-only (is-land-owner (land-id uint) (address principal))
    (match (map-get? LandRecords land-id)
        land-info (is-eq (get owner land-info) address)
        false
    )
)

;; Get total registered lands
(define-read-only (get-total-lands)
    (var-get land-id-nonce)
)

;; Get contract admin
(define-read-only (get-admin)
    (var-get admin)
)

