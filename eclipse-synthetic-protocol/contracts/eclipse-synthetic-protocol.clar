;; Eclipse Protocol - Synthetic Asset Platform
;; Core smart contract implementing synthetic asset pairs with dynamic collateralization

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-asset-not-found (err u102))
(define-constant err-invalid-oracle (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-pool-not-found (err u105))
(define-constant err-invalid-ratio (err u106))

;; Minimum collateralization ratio (150%)
(define-constant min-collateral-ratio u150)

;; Data Variables
(define-data-var protocol-paused bool false)
(define-data-var insurance-pool-balance uint u0)
(define-data-var total-ecl-supply uint u1000000000)

;; Data Maps
(define-map synthetic-assets
    { asset-id: (string-ascii 10) }
    {
        name: (string-ascii 50),
        total-supply: uint,
        collateral-ratio: uint,
        oracle-price: uint,
        is-active: bool
    }
)

(define-map user-positions
    { user: principal, asset-id: (string-ascii 10) }
    {
        collateral-amount: uint,
        synthetic-amount: uint,
        entry-price: uint
    }
)

(define-map liquidity-pools
    { pool-id: (string-ascii 10) }
    {
        asset-id: (string-ascii 10),
        liquidity: uint,
        shadow-token-supply: uint,
        volatility-factor: uint
    }
)

(define-map oracle-feeds
    { oracle-id: principal }
    {
        stake-weight: uint,
        accuracy-score: uint,
        is-authorized: bool
    }
)

(define-map user-balances
    { user: principal, asset-id: (string-ascii 10) }
    { balance: uint }
)

(define-map ecl-stakes
    { user: principal }
    {
        staked-amount: uint,
        reward-debt: uint,
        last-claim: uint
    }
)

;; Read-only functions
(define-read-only (get-synthetic-asset (asset-id (string-ascii 10)))
    (map-get? synthetic-assets { asset-id: asset-id })
)

(define-read-only (get-user-position (user principal) (asset-id (string-ascii 10)))
    (map-get? user-positions { user: user, asset-id: asset-id })
)

(define-read-only (get-user-balance (user principal) (asset-id (string-ascii 10)))
    (default-to 
        { balance: u0 }
        (map-get? user-balances { user: user, asset-id: asset-id })
    )
)

(define-read-only (get-liquidity-pool (pool-id (string-ascii 10)))
    (map-get? liquidity-pools { pool-id: pool-id })
)

(define-read-only (calculate-collateral-required (asset-id (string-ascii 10)) (synthetic-amount uint))
    (let
        (
            (asset (unwrap! (get-synthetic-asset asset-id) err-asset-not-found))
            (price (get oracle-price asset))
            (ratio (get collateral-ratio asset))
        )
        (ok (/ (* synthetic-amount price ratio) u100))
    )
)

(define-read-only (get-insurance-pool-balance)
    (ok (var-get insurance-pool-balance))
)

;; Public functions

;; Initialize a new synthetic asset
(define-public (create-synthetic-asset 
    (asset-id (string-ascii 10))
    (name (string-ascii 50))
    (initial-price uint)
    (collateral-ratio uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= collateral-ratio min-collateral-ratio) err-invalid-ratio)
        (ok (map-set synthetic-assets
            { asset-id: asset-id }
            {
                name: name,
                total-supply: u0,
                collateral-ratio: collateral-ratio,
                oracle-price: initial-price,
                is-active: true
            }
        ))
    )
)

;; Mint synthetic assets by providing collateral
(define-public (mint-synthetic 
    (asset-id (string-ascii 10))
    (collateral-amount uint)
    (synthetic-amount uint))
    (let
        (
            (asset (unwrap! (get-synthetic-asset asset-id) err-asset-not-found))
            (required-collateral (unwrap! (calculate-collateral-required asset-id synthetic-amount) err-asset-not-found))
            (current-position (default-to 
                { collateral-amount: u0, synthetic-amount: u0, entry-price: u0 }
                (get-user-position tx-sender asset-id)))
        )
        (asserts! (>= collateral-amount required-collateral) err-insufficient-collateral)
        (asserts! (get is-active asset) err-asset-not-found)
        
        ;; Update user position
        (map-set user-positions
            { user: tx-sender, asset-id: asset-id }
            {
                collateral-amount: (+ (get collateral-amount current-position) collateral-amount),
                synthetic-amount: (+ (get synthetic-amount current-position) synthetic-amount),
                entry-price: (get oracle-price asset)
            }
        )
        
        ;; Update user balance
        (let ((current-balance (get balance (get-user-balance tx-sender asset-id))))
            (map-set user-balances
                { user: tx-sender, asset-id: asset-id }
                { balance: (+ current-balance synthetic-amount) }
            )
        )
        
        ;; Update total supply
        (map-set synthetic-assets
            { asset-id: asset-id }
            (merge asset { total-supply: (+ (get total-supply asset) synthetic-amount) })
        )
        
        (ok true)
    )
)

;; Burn synthetic assets to reclaim collateral
(define-public (burn-synthetic 
    (asset-id (string-ascii 10))
    (synthetic-amount uint))
    (let
        (
            (position (unwrap! (get-user-position tx-sender asset-id) err-asset-not-found))
            (user-balance (get balance (get-user-balance tx-sender asset-id)))
            (asset (unwrap! (get-synthetic-asset asset-id) err-asset-not-found))
        )
        (asserts! (>= user-balance synthetic-amount) err-insufficient-balance)
        (asserts! (>= (get synthetic-amount position) synthetic-amount) err-insufficient-balance)
        
        ;; Calculate collateral to return
        (let ((collateral-to-return 
                (/ (* (get collateral-amount position) synthetic-amount) 
                   (get synthetic-amount position))))
            
            ;; Update position
            (map-set user-positions
                { user: tx-sender, asset-id: asset-id }
                {
                    collateral-amount: (- (get collateral-amount position) collateral-to-return),
                    synthetic-amount: (- (get synthetic-amount position) synthetic-amount),
                    entry-price: (get entry-price position)
                }
            )
            
            ;; Update balance
            (map-set user-balances
                { user: tx-sender, asset-id: asset-id }
                { balance: (- user-balance synthetic-amount) }
            )
            
            ;; Update total supply
            (map-set synthetic-assets
                { asset-id: asset-id }
                (merge asset { total-supply: (- (get total-supply asset) synthetic-amount) })
            )
            
            (ok collateral-to-return)
        )
    )
)

;; Create a liquidity pool for a synthetic asset
(define-public (create-liquidity-pool
    (pool-id (string-ascii 10))
    (asset-id (string-ascii 10))
    (initial-liquidity uint))
    (begin
        (asserts! (is-some (get-synthetic-asset asset-id)) err-asset-not-found)
        (ok (map-set liquidity-pools
            { pool-id: pool-id }
            {
                asset-id: asset-id,
                liquidity: initial-liquidity,
                shadow-token-supply: initial-liquidity,
                volatility-factor: u100
            }
        ))
    )
)

;; Update oracle price (authorized oracles only)
(define-public (update-oracle-price
    (asset-id (string-ascii 10))
    (new-price uint))
    (let
        (
            (oracle (unwrap! (map-get? oracle-feeds { oracle-id: tx-sender }) err-invalid-oracle))
            (asset (unwrap! (get-synthetic-asset asset-id) err-asset-not-found))
        )
        (asserts! (get is-authorized oracle) err-invalid-oracle)
        (ok (map-set synthetic-assets
            { asset-id: asset-id }
            (merge asset { oracle-price: new-price })
        ))
    )
)

;; Authorize oracle feed
(define-public (authorize-oracle
    (oracle-id principal)
    (stake-weight uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set oracle-feeds
            { oracle-id: oracle-id }
            {
                stake-weight: stake-weight,
                accuracy-score: u100,
                is-authorized: true
            }
        ))
    )
)

;; Stake ECL governance tokens
(define-public (stake-ecl (amount uint))
    (let
        (
            (current-stake (default-to
                { staked-amount: u0, reward-debt: u0, last-claim: u0 }
                (map-get? ecl-stakes { user: tx-sender })))
        )
        (ok (map-set ecl-stakes
            { user: tx-sender }
            {
                staked-amount: (+ (get staked-amount current-stake) amount),
                reward-debt: (get reward-debt current-stake),
                last-claim: block-height
            }
        ))
    )
)

;; Contribute to insurance pool
(define-public (contribute-to-insurance (amount uint))
    (begin
        (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) amount))
        (ok true)
    )
)

;; Update collateral ratio based on market conditions
(define-public (update-collateral-ratio
    (asset-id (string-ascii 10))
    (new-ratio uint))
    (let
        (
            (asset (unwrap! (get-synthetic-asset asset-id) err-asset-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= new-ratio min-collateral-ratio) err-invalid-ratio)
        (ok (map-set synthetic-assets
            { asset-id: asset-id }
            (merge asset { collateral-ratio: new-ratio })
        ))
    )
)

;; Emergency pause
(define-public (toggle-pause)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set protocol-paused (not (var-get protocol-paused))))
    )
)