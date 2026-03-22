;; BounceCache - Blockchain Gaming Ecosystem
;; Implements: NFT avatars, Skill DNA, Bounce Pool rewards, tournaments, governance

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-INVALID-PARAM (err u105))
(define-constant ERR-TOURNAMENT-CLOSED (err u106))
(define-constant ERR-ALREADY-REGISTERED (err u107))
(define-constant ERR-VOTING-CLOSED (err u108))
(define-constant ERR-ALREADY-VOTED (err u109))

;; Skill DNA attribute indices
(define-constant ATTR-SPEED u0)
(define-constant ATTR-ACCURACY u1)
(define-constant ATTR-STRATEGY u2)
(define-constant ATTR-ENDURANCE u3)

;; Max values
(define-constant MAX-SKILL-VALUE u100)
(define-constant MAX-LEVEL u50)
(define-constant BOUNCE-POOL-FEE-BPS u250) ;; 2.5% of entry fees go to bounce pool
(define-constant EMERGING-PLAYER-SHARE-BPS u500) ;; 5% of bounce pool to emerging players

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var next-avatar-id uint u1)
(define-data-var next-tournament-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var bounce-pool-balance uint u0)
(define-data-var platform-paused bool false)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; NFT Avatar ownership
(define-map avatars
  { avatar-id: uint }
  {
    owner: principal,
    level: uint,
    xp: uint,
    games-played: uint,
    wins: uint,
    ;; Skill DNA: four attributes stored as a list
    speed: uint,
    accuracy: uint,
    strategy: uint,
    endurance: uint,
    ;; Cross-game interoperability tag (game-mode-id)
    primary-game-mode: uint,
    created-at: uint
  }
)

;; Map owner -> list of owned avatar IDs (max 10 per owner for simplicity)
(define-map owner-avatars
  { owner: principal }
  { avatar-ids: (list 10 uint) }
)

;; Tournament registry
(define-map tournaments
  { tournament-id: uint }
  {
    creator: principal,
    name: (string-ascii 64),
    game-mode-id: uint,
    entry-fee: uint,         ;; in uSTX
    prize-pool: uint,        ;; accumulated from entry fees minus bounce-pool cut
    max-players: uint,
    registered-count: uint,
    is-open: bool,
    winner: (optional principal),
    created-at: uint
  }
)

;; Tournament registrations
(define-map tournament-registrations
  { tournament-id: uint, player: principal }
  { avatar-id: uint, registered-at: uint }
)

;; Governance proposals
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 128),
    description: (string-ascii 256),
    votes-for: uint,
    votes-against: uint,
    is-active: bool,
    created-at: uint,
    end-block: uint
  }
)

;; Vote tracking
(define-map votes
  { proposal-id: uint, voter: principal }
  { voted-for: bool }
)

;; Player performance proofs submitted by the contract owner (oracle)
(define-map performance-proofs
  { avatar-id: uint, game-session: uint }
  {
    score: uint,
    accuracy-delta: int,
    speed-delta: int,
    strategy-delta: int,
    endurance-delta: int,
    xp-earned: uint,
    verified-at: uint
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (clamp-skill (current uint) (delta int))
  (let (
    (raw (+ (to-int current) delta))
  )
    (if (< raw 0)
      u0
      (if (> (to-uint raw) MAX-SKILL-VALUE)
        MAX-SKILL-VALUE
        (to-uint raw)
      )
    )
  )
)

(define-private (xp-for-next-level (level uint))
  ;; XP threshold grows linearly: 1000 * (level + 1)
  (* u1000 (+ level u1))
)

(define-private (apply-level-up (avatar-id uint) (current-xp uint) (current-level uint))
  (let (
    (threshold (xp-for-next-level current-level))
  )
    (if (and (>= current-xp threshold) (< current-level MAX-LEVEL))
      ;; Level up: consume threshold XP, increment level
      (begin
        (match (map-get? avatars { avatar-id: avatar-id })
          avatar (map-set avatars { avatar-id: avatar-id }
            (merge avatar {
              level: (+ current-level u1),
              xp: (- current-xp threshold)
            })
          )
          false
        )
        true
      )
      false
    )
  )
)

(define-private (calculate-bounce-cut (amount uint))
  (/ (* amount BOUNCE-POOL-FEE-BPS) u10000)
)

;; ============================================================
;; PUBLIC FUNCTIONS - AVATAR (NFT)
;; ============================================================

;; Mint a new avatar NFT for the caller
(define-public (mint-avatar (game-mode-id uint))
  (let (
    (avatar-id (var-get next-avatar-id))
    (caller tx-sender)
    (existing (map-get? owner-avatars { owner: caller }))
    (current-ids (match existing e (get avatar-ids e) (list)))
  )
    (asserts! (not (var-get platform-paused)) ERR-UNAUTHORIZED)
    (asserts! (< (len current-ids) u10) ERR-INVALID-PARAM)

    (map-set avatars { avatar-id: avatar-id }
      {
        owner: caller,
        level: u1,
        xp: u0,
        games-played: u0,
        wins: u0,
        speed: u10,
        accuracy: u10,
        strategy: u10,
        endurance: u10,
        primary-game-mode: game-mode-id,
        created-at: block-height
      }
    )
    (map-set owner-avatars { owner: caller }
      { avatar-ids: (unwrap! (as-max-len? (append current-ids avatar-id) u10) ERR-INVALID-PARAM) }
    )
    (var-set next-avatar-id (+ avatar-id u1))
    (ok avatar-id)
  )
)

;; Transfer avatar ownership
(define-public (transfer-avatar (avatar-id uint) (recipient principal))
  (let (
    (avatar (unwrap! (map-get? avatars { avatar-id: avatar-id }) ERR-NOT-FOUND))
    (caller tx-sender)
    (sender-entry (unwrap! (map-get? owner-avatars { owner: caller }) ERR-NOT-FOUND))
    (recipient-entry (map-get? owner-avatars { owner: recipient }))
    (recipient-ids (match recipient-entry e (get avatar-ids e) (list)))
  )
    (asserts! (is-eq (get owner avatar) caller) ERR-UNAUTHORIZED)
    (asserts! (< (len recipient-ids) u10) ERR-INVALID-PARAM)

    ;; Update avatar owner
    (map-set avatars { avatar-id: avatar-id }
      (merge avatar { owner: recipient })
    )
    ;; Update recipient list
    (map-set owner-avatars { owner: recipient }
      { avatar-ids: (unwrap! (as-max-len? (append recipient-ids avatar-id) u10) ERR-INVALID-PARAM) }
    )
    (ok true)
  )
)

;; ============================================================
;; PUBLIC FUNCTIONS - PERFORMANCE PROOF (Oracle submission)
;; ============================================================

;; Contract owner submits a verified performance proof for a game session
(define-public (submit-performance-proof
  (avatar-id uint)
  (game-session uint)
  (score uint)
  (acc-delta int)
  (spd-delta int)
  (str-delta int)
  (end-delta int)
  (xp-earned uint)
  (is-win bool)
)
  (let (
    (avatar (unwrap! (map-get? avatars { avatar-id: avatar-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (asserts! (is-none (map-get? performance-proofs { avatar-id: avatar-id, game-session: game-session })) ERR-ALREADY-EXISTS)

    ;; Store the proof
    (map-set performance-proofs { avatar-id: avatar-id, game-session: game-session }
      {
        score: score,
        accuracy-delta: acc-delta,
        speed-delta: spd-delta,
        strategy-delta: str-delta,
        endurance-delta: end-delta,
        xp-earned: xp-earned,
        verified-at: block-height
      }
    )

    ;; Apply Skill DNA evolution
    (let (
      (new-speed    (clamp-skill (get speed avatar)    spd-delta))
      (new-accuracy (clamp-skill (get accuracy avatar) acc-delta))
      (new-strategy (clamp-skill (get strategy avatar) str-delta))
      (new-endurance (clamp-skill (get endurance avatar) end-delta))
      (new-xp (+ (get xp avatar) xp-earned))
      (new-games (+ (get games-played avatar) u1))
      (new-wins (if is-win (+ (get wins avatar) u1) (get wins avatar)))
      (new-level (get level avatar))
    )
      (map-set avatars { avatar-id: avatar-id }
        (merge avatar {
          speed: new-speed,
          accuracy: new-accuracy,
          strategy: new-strategy,
          endurance: new-endurance,
          xp: new-xp,
          games-played: new-games,
          wins: new-wins
        })
      )
      ;; Attempt level-up after updating XP
      (applyLevelUpCheck avatar-id new-xp new-level)
      (ok true)
    )
  )
)

;; Internal level-up trigger (named for clarity in call stack)
(define-private (applyLevelUpCheck (avatar-id uint) (xp uint) (level uint))
  (apply-level-up avatar-id xp level)
)

;; ============================================================
;; PUBLIC FUNCTIONS - TOURNAMENTS
;; ============================================================

;; Create a new community tournament
(define-public (create-tournament
  (name (string-ascii 64))
  (game-mode-id uint)
  (entry-fee uint)
  (max-players uint)
)
  (let (
    (tournament-id (var-get next-tournament-id))
  )
    (asserts! (not (var-get platform-paused)) ERR-UNAUTHORIZED)
    (asserts! (> max-players u1) ERR-INVALID-PARAM)
    (asserts! (> (len name) u0) ERR-INVALID-PARAM)

    (map-set tournaments { tournament-id: tournament-id }
      {
        creator: tx-sender,
        name: name,
        game-mode-id: game-mode-id,
        entry-fee: entry-fee,
        prize-pool: u0,
        max-players: max-players,
        registered-count: u0,
        is-open: true,
        winner: none,
        created-at: block-height
      }
    )
    (var-set next-tournament-id (+ tournament-id u1))
    (ok tournament-id)
  )
)

;; Register for a tournament - pays entry fee in STX
(define-public (register-for-tournament (tournament-id uint) (avatar-id uint))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) ERR-NOT-FOUND))
    (avatar (unwrap! (map-get? avatars { avatar-id: avatar-id }) ERR-NOT-FOUND))
    (caller tx-sender)
    (entry-fee (get entry-fee tournament))
    (bounce-cut (calculate-bounce-cut entry-fee))
    (net-to-pool (- entry-fee bounce-cut))
  )
    (asserts! (get is-open tournament) ERR-TOURNAMENT-CLOSED)
    (asserts! (is-eq (get owner avatar) caller) ERR-UNAUTHORIZED)
    (asserts! (< (get registered-count tournament) (get max-players tournament)) ERR-TOURNAMENT-CLOSED)
    (asserts! (is-none (map-get? tournament-registrations { tournament-id: tournament-id, player: caller })) ERR-ALREADY-REGISTERED)

    ;; Collect entry fee in uSTX
    (if (> entry-fee u0)
      (begin
        (try! (stx-transfer? entry-fee caller (as-contract tx-sender)))
        ;; Accumulate bounce pool cut
        (var-set bounce-pool-balance (+ (var-get bounce-pool-balance) bounce-cut))
        ;; Update tournament prize pool
        (map-set tournaments { tournament-id: tournament-id }
          (merge tournament {
            prize-pool: (+ (get prize-pool tournament) net-to-pool),
            registered-count: (+ (get registered-count tournament) u1)
          })
        )
      )
      ;; Free tournament: just increment count
      (map-set tournaments { tournament-id: tournament-id }
        (merge tournament { registered-count: (+ (get registered-count tournament) u1) })
      )
    )

    (map-set tournament-registrations { tournament-id: tournament-id, player: caller }
      { avatar-id: avatar-id, registered-at: block-height }
    )
    (ok true)
  )
)

;; Declare tournament winner and distribute prize (owner/creator only)
(define-public (declare-winner (tournament-id uint) (winner principal))
  (let (
    (tournament (unwrap! (map-get? tournaments { tournament-id: tournament-id }) ERR-NOT-FOUND))
    (caller tx-sender)
  )
    (asserts! (or (is-eq caller CONTRACT-OWNER) (is-eq caller (get creator tournament))) ERR-UNAUTHORIZED)
    (asserts! (get is-open tournament) ERR-TOURNAMENT-CLOSED)
    (asserts! (is-some (map-get? tournament-registrations { tournament-id: tournament-id, player: winner })) ERR-NOT-FOUND)

    ;; Send prize pool to winner
    (let ((prize (get prize-pool tournament)))
      (if (> prize u0)
        (try! (as-contract (stx-transfer? prize tx-sender winner)))
        true
      )
    )

    ;; Close tournament and record winner
    (map-set tournaments { tournament-id: tournament-id }
      (merge tournament { is-open: false, winner: (some winner), prize-pool: u0 })
    )
    (ok true)
  )
)

;; ============================================================
;; PUBLIC FUNCTIONS - BOUNCE POOL (emerging player support)
;; ============================================================

;; Distribute bounce pool share to an emerging player (low level, verified by owner)
(define-public (distribute-bounce-pool (recipient principal) (amount uint))
  (let (
    (pool (var-get bounce-pool-balance))
    (max-distribution (/ (* pool EMERGING-PLAYER-SHARE-BPS) u10000))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (asserts! (<= amount max-distribution) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> amount u0) ERR-INVALID-PARAM)

    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set bounce-pool-balance (- pool amount))
    (ok true)
  )
)

;; ============================================================
;; PUBLIC FUNCTIONS - GOVERNANCE
;; ============================================================

;; Create a governance proposal (any avatar holder)
(define-public (create-proposal
  (title (string-ascii 128))
  (description (string-ascii 256))
  (duration-blocks uint)
)
  (let (
    (proposal-id (var-get next-proposal-id))
    (caller tx-sender)
    (owner-entry (map-get? owner-avatars { owner: caller }))
  )
    ;; Must hold at least one avatar to propose
    (asserts! (is-some owner-entry) ERR-UNAUTHORIZED)
    (asserts! (> (len (get avatar-ids (unwrap! owner-entry ERR-UNAUTHORIZED))) u0) ERR-UNAUTHORIZED)
    (asserts! (> duration-blocks u0) ERR-INVALID-PARAM)

    (map-set proposals { proposal-id: proposal-id }
      {
        proposer: caller,
        title: title,
        description: description,
        votes-for: u0,
        votes-against: u0,
        is-active: true,
        created-at: block-height,
        end-block: (+ block-height duration-blocks)
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Cast a vote on an active proposal (one vote per avatar holder)
(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (caller tx-sender)
    (owner-entry (map-get? owner-avatars { owner: caller }))
  )
    (asserts! (is-some owner-entry) ERR-UNAUTHORIZED)
    (asserts! (get is-active proposal) ERR-VOTING-CLOSED)
    (asserts! (<= block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: caller })) ERR-ALREADY-VOTED)

    (map-set votes { proposal-id: proposal-id, voter: caller }
      { voted-for: vote-for }
    )

    (if vote-for
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) u1) })
      )
      (map-set proposals { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) u1) })
      )
    )
    (ok true)
  )
)

;; Close a proposal after its end block (anyone can finalize)
(define-public (close-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
  )
    (asserts! (get is-active proposal) ERR-VOTING-CLOSED)
    (asserts! (> block-height (get end-block proposal)) ERR-INVALID-PARAM)

    (map-set proposals { proposal-id: proposal-id }
      (merge proposal { is-active: false })
    )
    (ok true)
  )
)

;; ============================================================
;; ADMIN FUNCTIONS
;; ============================================================

(define-public (set-platform-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (var-set platform-paused paused)
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-avatar (avatar-id uint))
  (map-get? avatars { avatar-id: avatar-id })
)

(define-read-only (get-owner-avatars (owner principal))
  (map-get? owner-avatars { owner: owner })
)

(define-read-only (get-tournament (tournament-id uint))
  (map-get? tournaments { tournament-id: tournament-id })
)

(define-read-only (get-tournament-registration (tournament-id uint) (player principal))
  (map-get? tournament-registrations { tournament-id: tournament-id, player: player })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-bounce-pool-balance)
  (var-get bounce-pool-balance)
)

(define-read-only (get-performance-proof (avatar-id uint) (game-session uint))
  (map-get? performance-proofs { avatar-id: avatar-id, game-session: game-session })
)

(define-read-only (get-avatar-skill-dna (avatar-id uint))
  (match (map-get? avatars { avatar-id: avatar-id })
    avatar (some {
      speed: (get speed avatar),
      accuracy: (get accuracy avatar),
      strategy: (get strategy avatar),
      endurance: (get endurance avatar),
      level: (get level avatar),
      xp: (get xp avatar)
    })
    none
  )
)

(define-read-only (get-platform-stats)
  {
    total-avatars: (- (var-get next-avatar-id) u1),
    total-tournaments: (- (var-get next-tournament-id) u1),
    total-proposals: (- (var-get next-proposal-id) u1),
    bounce-pool: (var-get bounce-pool-balance),
    is-paused: (var-get platform-paused)
  }
)
