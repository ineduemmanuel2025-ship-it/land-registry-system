
;; Dispute Resolution System
;; Handles land ownership disputes with mediation and arbitration functionality

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_DISPUTE_NOT_FOUND (err u201))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_STATUS (err u203))
(define-constant ERR_INSUFFICIENT_EVIDENCE (err u204))
(define-constant ERR_VOTING_CLOSED (err u205))
(define-constant ERR_ALREADY_VOTED (err u206))
(define-constant ERR_NOT_MEDIATOR (err u207))

;; Data Variables
(define-data-var dispute-id-nonce uint u0)
(define-data-var admin principal CONTRACT_OWNER)
(define-data-var mediation-fee uint u1000000) ;; 1 STX in microSTX

;; Data Maps
(define-map Disputes
    uint
    {
        land-id: uint,
        plaintiff: principal,
        defendant: principal,
        dispute-type: (string-ascii 50),
        description: (string-ascii 500),
        status: (string-ascii 20),
        filed-at: uint,
        resolved-at: (optional uint),
        resolution: (optional (string-ascii 500)),
        mediator: (optional principal)
    }
)

(define-map Evidence
    {dispute-id: uint, evidence-id: uint}
    {
        submitter: principal,
        evidence-hash: (string-ascii 64),
        description: (string-ascii 200),
        submitted-at: uint
    }
)

(define-map Mediators
    principal
    {
        active: bool,
        reputation-score: uint,
        cases-resolved: uint,
        registered-at: uint
    }
)

(define-map MediationVotes
    {dispute-id: uint, voter: principal}
    {
        vote: (string-ascii 20),
        reasoning: (string-ascii 300),
        voted-at: uint
    }
)

(define-map DisputeStats
    uint
    {
        evidence-count: uint,
        vote-count: uint,
        votes-for-plaintiff: uint,
        votes-for-defendant: uint
    }
)

;; Public Functions

;; File a new dispute
(define-public (file-dispute 
    (land-id uint) 
    (defendant principal) 
    (dispute-type (string-ascii 50)) 
    (description (string-ascii 500))
)
    (let (
        (new-dispute-id (+ (var-get dispute-id-nonce) u1))
        (current-block-height stacks-block-height)
    )
        (map-set Disputes new-dispute-id {
            land-id: land-id,
            plaintiff: tx-sender,
            defendant: defendant,
            dispute-type: dispute-type,
            description: description,
            status: "pending",
            filed-at: current-block-height,
            resolved-at: none,
            resolution: none,
            mediator: none
        })
        
        (map-set DisputeStats new-dispute-id {
            evidence-count: u0,
            vote-count: u0,
            votes-for-plaintiff: u0,
            votes-for-defendant: u0
        })
        
        (var-set dispute-id-nonce new-dispute-id)
        (ok new-dispute-id)
    )
)

;; Submit evidence for a dispute
(define-public (submit-evidence 
    (dispute-id uint) 
    (evidence-hash (string-ascii 64)) 
    (description (string-ascii 200))
)
    (let (
        (dispute-info (unwrap! (map-get? Disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
        (stats (unwrap! (map-get? DisputeStats dispute-id) ERR_DISPUTE_NOT_FOUND))
        (evidence-id (+ (get evidence-count stats) u1))
    )
        (asserts! (or 
            (is-eq tx-sender (get plaintiff dispute-info))
            (is-eq tx-sender (get defendant dispute-info))
        ) ERR_UNAUTHORIZED)
        
        (asserts! (is-eq (get status dispute-info) "pending") ERR_INVALID_STATUS)
        
        (map-set Evidence {dispute-id: dispute-id, evidence-id: evidence-id} {
            submitter: tx-sender,
            evidence-hash: evidence-hash,
            description: description,
            submitted-at: stacks-block-height
        })
        
        (map-set DisputeStats dispute-id 
            (merge stats {
                evidence-count: evidence-id
            })
        )
        
        (ok evidence-id)
    )
)

;; Register as mediator
(define-public (register-mediator)
    (begin
        (map-set Mediators tx-sender {
            active: true,
            reputation-score: u100,
            cases-resolved: u0,
            registered-at: stacks-block-height
        })
        
        (ok true)
    )
)

;; Assign mediator to dispute (admin only)
(define-public (assign-mediator (dispute-id uint) (mediator principal))
    (let (
        (dispute-info (unwrap! (map-get? Disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
        (mediator-info (unwrap! (map-get? Mediators mediator) ERR_NOT_MEDIATOR))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
        (asserts! (get active mediator-info) ERR_NOT_MEDIATOR)
        (asserts! (is-eq (get status dispute-info) "pending") ERR_INVALID_STATUS)
        
        (map-set Disputes dispute-id 
            (merge dispute-info {
                status: "in-mediation",
                mediator: (some mediator)
            })
        )
        
        (ok true)
    )
)

;; Vote on dispute resolution (mediator only)
(define-public (vote-on-dispute 
    (dispute-id uint) 
    (vote (string-ascii 20)) 
    (reasoning (string-ascii 300))
)
    (let (
        (dispute-info (unwrap! (map-get? Disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
        (stats (unwrap! (map-get? DisputeStats dispute-id) ERR_DISPUTE_NOT_FOUND))
    )
        (asserts! (is-some (get mediator dispute-info)) ERR_NOT_MEDIATOR)
        (asserts! (is-eq tx-sender (unwrap-panic (get mediator dispute-info))) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status dispute-info) "in-mediation") ERR_VOTING_CLOSED)
        
        (asserts! (is-none (map-get? MediationVotes {dispute-id: dispute-id, voter: tx-sender})) ERR_ALREADY_VOTED)
        
        (map-set MediationVotes {dispute-id: dispute-id, voter: tx-sender} {
            vote: vote,
            reasoning: reasoning,
            voted-at: stacks-block-height
        })
        
        (let (
            (new-vote-count (+ (get vote-count stats) u1))
            (new-plaintiff-votes (if (is-eq vote "plaintiff") 
                (+ (get votes-for-plaintiff stats) u1) 
                (get votes-for-plaintiff stats)
            ))
            (new-defendant-votes (if (is-eq vote "defendant") 
                (+ (get votes-for-defendant stats) u1) 
                (get votes-for-defendant stats)
            ))
        )
            (map-set DisputeStats dispute-id {
                evidence-count: (get evidence-count stats),
                vote-count: new-vote-count,
                votes-for-plaintiff: new-plaintiff-votes,
                votes-for-defendant: new-defendant-votes
            })
        )
        
        (ok true)
    )
)

;; Resolve dispute (admin only)
(define-public (resolve-dispute 
    (dispute-id uint) 
    (resolution (string-ascii 500))
)
    (let (
        (dispute-info (unwrap! (map-get? Disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
        (mediator (unwrap! (get mediator dispute-info) ERR_NOT_MEDIATOR))
        (mediator-info (unwrap! (map-get? Mediators mediator) ERR_NOT_MEDIATOR))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
        (asserts! (or 
            (is-eq (get status dispute-info) "in-mediation")
            (is-eq (get status dispute-info) "pending")
        ) ERR_INVALID_STATUS)
        
        (map-set Disputes dispute-id 
            (merge dispute-info {
                status: "resolved",
                resolved-at: (some stacks-block-height),
                resolution: (some resolution)
            })
        )
        
        ;; Update mediator reputation
        (map-set Mediators mediator 
            (merge mediator-info {
                cases-resolved: (+ (get cases-resolved mediator-info) u1),
                reputation-score: (+ (get reputation-score mediator-info) u10)
            })
        )
        
        (ok true)
    )
)

;; Read-Only Functions

;; Get dispute information
(define-read-only (get-dispute-info (dispute-id uint))
    (map-get? Disputes dispute-id)
)

;; Get dispute statistics
(define-read-only (get-dispute-stats (dispute-id uint))
    (map-get? DisputeStats dispute-id)
)

;; Get evidence for dispute
(define-read-only (get-evidence (dispute-id uint) (evidence-id uint))
    (map-get? Evidence {dispute-id: dispute-id, evidence-id: evidence-id})
)

;; Get mediator information
(define-read-only (get-mediator-info (mediator principal))
    (map-get? Mediators mediator)
)

;; Get mediation vote
(define-read-only (get-mediation-vote (dispute-id uint) (voter principal))
    (map-get? MediationVotes {dispute-id: dispute-id, voter: voter})
)

;; Check if address is involved in dispute
(define-read-only (is-dispute-party (dispute-id uint) (address principal))
    (match (map-get? Disputes dispute-id)
        dispute-info (or 
            (is-eq (get plaintiff dispute-info) address)
            (is-eq (get defendant dispute-info) address)
        )
        false
    )
)

;; Get total disputes filed
(define-read-only (get-total-disputes)
    (var-get dispute-id-nonce)
)

;; Get mediation fee
(define-read-only (get-mediation-fee)
    (var-get mediation-fee)
)

