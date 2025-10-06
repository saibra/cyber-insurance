;; CyberGuard Protocol - Decentralized Cyber Insurance
;; A parametric insurance protocol for DeFi exploits and smart contract failures

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-coverage-expired (err u105))
(define-constant err-claim-already-processed (err u106))
(define-constant err-insufficient-coverage (err u107))
(define-constant err-invalid-risk-score (err u108))

;; Data Variables
(define-data-var protocol-paused bool false)
(define-data-var total-coverage-issued uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var minimum-premium uint u100)
(define-data-var claim-counter uint u0)
(define-data-var coverage-counter uint u0)

;; Data Maps

;; Coverage Policies
(define-map coverage-policies
    uint
    {
        holder: principal,
        protocol-name: (string-ascii 50),
        coverage-amount: uint,
        premium-paid: uint,
        start-block: uint,
        end-block: uint,
        risk-score: uint,
        active: bool,
        tvl: uint
    }
)

;; Claims
(define-map claims
    uint
    {
        coverage-id: uint,
        claimant: principal,
        claim-amount: uint,
        claim-block: uint,
        status: (string-ascii 20),
        exploit-type: (string-ascii 50),
        processed: bool,
        approved: bool
    }
)

;; User balances for different tokens
(define-map guard-balances principal uint)
(define-map shield-balances principal uint)
(define-map claim-token-balances principal uint)

;; Protocol risk scores (0-100)
(define-map protocol-risk-scores (string-ascii 50) uint)

;; Security researcher rewards
(define-map researcher-rewards principal uint)

;; Staked collateral
(define-map staked-collateral principal uint)

;; Read-only functions

(define-read-only (get-coverage-policy (coverage-id uint))
    (map-get? coverage-policies coverage-id)
)

(define-read-only (get-claim (claim-id uint))
    (map-get? claims claim-id)
)

(define-read-only (get-guard-balance (account principal))
    (default-to u0 (map-get? guard-balances account))
)

(define-read-only (get-shield-balance (account principal))
    (default-to u0 (map-get? shield-balances account))
)

(define-read-only (get-claim-token-balance (account principal))
    (default-to u0 (map-get? claim-token-balances account))
)

(define-read-only (get-protocol-risk-score (protocol (string-ascii 50)))
    (default-to u50 (map-get? protocol-risk-scores protocol))
)

(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)

(define-read-only (is-protocol-paused)
    (var-get protocol-paused)
)

(define-read-only (get-total-coverage-issued)
    (var-get total-coverage-issued)
)

(define-read-only (get-total-claims-paid)
    (var-get total-claims-paid)
)

;; Private functions

(define-private (calculate-premium (coverage-amount uint) (risk-score uint) (duration uint))
    (let
        (
            (base-rate (/ (* coverage-amount u5) u1000))
            (risk-multiplier (/ risk-score u50))
            (duration-factor (/ duration u4320))
        )
        (+ base-rate (* base-rate risk-multiplier duration-factor))
    )
)

;; Public functions

;; Initialize GUARD tokens for a user
(define-public (mint-guard-tokens (recipient principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (map-set guard-balances recipient 
            (+ (get-guard-balance recipient) amount))
        (ok true)
    )
)

;; Initialize SHIELD tokens for liquidity providers
(define-public (mint-shield-tokens (recipient principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (map-set shield-balances recipient 
            (+ (get-shield-balance recipient) amount))
        (ok true)
    )
)

;; Update protocol risk score
(define-public (update-risk-score (protocol (string-ascii 50)) (score uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= score u100) err-invalid-risk-score)
        (map-set protocol-risk-scores protocol score)
        (ok true)
    )
)

;; Purchase coverage policy
(define-public (purchase-coverage 
    (protocol-name (string-ascii 50))
    (coverage-amount uint)
    (duration uint)
    (tvl uint))
    (let
        (
            (risk-score (get-protocol-risk-score protocol-name))
            (premium (calculate-premium coverage-amount risk-score duration))
            (new-id (+ (var-get coverage-counter) u1))
            (guard-balance (get-guard-balance tx-sender))
        )
        (asserts! (not (var-get protocol-paused)) err-owner-only)
        (asserts! (>= guard-balance premium) err-insufficient-balance)
        (asserts! (> coverage-amount u0) err-invalid-amount)
        
        ;; Deduct premium from GUARD balance
        (map-set guard-balances tx-sender (- guard-balance premium))
        
        ;; Add to treasury
        (var-set treasury-balance (+ (var-get treasury-balance) premium))
        
        ;; Create coverage policy
        (map-set coverage-policies new-id {
            holder: tx-sender,
            protocol-name: protocol-name,
            coverage-amount: coverage-amount,
            premium-paid: premium,
            start-block: block-height,
            end-block: (+ block-height duration),
            risk-score: risk-score,
            active: true,
            tvl: tvl
        })
        
        (var-set coverage-counter new-id)
        (var-set total-coverage-issued 
            (+ (var-get total-coverage-issued) coverage-amount))
        
        (ok new-id)
    )
)

;; File a claim
(define-public (file-claim 
    (coverage-id uint)
    (claim-amount uint)
    (exploit-type (string-ascii 50)))
    (let
        (
            (coverage (unwrap! (map-get? coverage-policies coverage-id) err-not-found))
            (new-claim-id (+ (var-get claim-counter) u1))
        )
        (asserts! (is-eq tx-sender (get holder coverage)) err-owner-only)
        (asserts! (get active coverage) err-coverage-expired)
        (asserts! (<= block-height (get end-block coverage)) err-coverage-expired)
        (asserts! (<= claim-amount (get coverage-amount coverage)) err-insufficient-coverage)
        
        ;; Create claim
        (map-set claims new-claim-id {
            coverage-id: coverage-id,
            claimant: tx-sender,
            claim-amount: claim-amount,
            claim-block: block-height,
            status: "pending",
            exploit-type: exploit-type,
            processed: false,
            approved: false
        })
        
        (var-set claim-counter new-claim-id)
        (ok new-claim-id)
    )
)

;; Process claim (owner/oracle)
(define-public (process-claim (claim-id uint) (approved bool))
    (let
        (
            (claim (unwrap! (map-get? claims claim-id) err-not-found))
            (coverage-id (get coverage-id claim))
            (coverage (unwrap! (map-get? coverage-policies coverage-id) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (get processed claim)) err-claim-already-processed)
        
        (if approved
            (begin
                ;; Pay out claim
                (map-set guard-balances (get claimant claim)
                    (+ (get-guard-balance (get claimant claim)) (get claim-amount claim)))
                (var-set treasury-balance 
                    (- (var-get treasury-balance) (get claim-amount claim)))
                (var-set total-claims-paid 
                    (+ (var-get total-claims-paid) (get claim-amount claim)))
                
                ;; Deactivate coverage
                (map-set coverage-policies coverage-id
                    (merge coverage { active: false }))
            )
            true
        )
        
        ;; Update claim status
        (map-set claims claim-id
            (merge claim {
                processed: true,
                approved: approved,
                status: (if approved "approved" "rejected")
            }))
        
        (ok approved)
    )
)

;; Stake collateral
(define-public (stake-collateral (amount uint))
    (let
        (
            (current-stake (default-to u0 (map-get? staked-collateral tx-sender)))
            (guard-balance (get-guard-balance tx-sender))
        )
        (asserts! (>= guard-balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Deduct from balance and add to stake
        (map-set guard-balances tx-sender (- guard-balance amount))
        (map-set staked-collateral tx-sender (+ current-stake amount))
        
        ;; Mint SHIELD tokens as receipt
        (map-set shield-balances tx-sender 
            (+ (get-shield-balance tx-sender) amount))
        
        (ok true)
    )
)

;; Reward security researchers
(define-public (reward-researcher (researcher principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Mint CLAIM tokens
        (map-set claim-token-balances researcher 
            (+ (get-claim-token-balance researcher) amount))
        
        ;; Track rewards
        (map-set researcher-rewards researcher
            (+ (default-to u0 (map-get? researcher-rewards researcher)) amount))
        
        (ok true)
    )
)

;; Emergency pause
(define-public (pause-protocol)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set protocol-paused true)
        (ok true)
    )
)

;; Unpause protocol
(define-public (unpause-protocol)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set protocol-paused false)
        (ok true)
    )
)

;; Transfer GUARD tokens
(define-public (transfer-guard (recipient principal) (amount uint))
    (let
        (
            (sender-balance (get-guard-balance tx-sender))
        )
        (asserts! (>= sender-balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        (map-set guard-balances tx-sender (- sender-balance amount))
        (map-set guard-balances recipient (+ (get-guard-balance recipient) amount))
        
        (ok true)
    )
)

;; Initialize contract
(define-public (initialize)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set treasury-balance u0)
        (ok true)
    )
)
