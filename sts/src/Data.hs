module Data where

data Battle = Battle
  { _player   :: Unit
  , _enemies  :: [Unit]
  , _mana     :: Int
  , _turn     :: Int
  , _cardPool :: CardPool
  }

data Unit = Unit
  { _name       :: Name
  , _maxHealth  :: Int
  , _health     :: Int
  , _block      :: Int
  , _strength   :: Int
  , _dexterity  :: Int
  , _weak       :: Int
  , _vulnerable :: Int
  , _frail      :: Int
  , _intends    :: Int -> [Intend]
  }

data CardPool = CardPool
  { _drawPile    :: [Card]
  , _hand        :: [Card]
  , _discardPile :: [Card]
  }

data Name = 
  Ironclad | Huntress | Defect | Watcher |
  Cultist | JawWorm | 
  MadGremlin | FatGremlin | SneakyGremlin | GremlinWizard | ShieldGremlin
  deriving (Show, Eq)

data Card = Card
  { _cardName :: CardName
  , _cardType :: CardType
  , _cardCost :: Int
  , _cardInfo :: String
  }

data CardName = Strike | Defend | Bash
  deriving (Show, Eq)

data CardType = Attack | Skill | Power
  deriving (Show, Eq)

data Intend
  = NoIntend 
  | DealDamage Int
  | ApplyVulnerable Int
  | ApplyWeak Int
  | ApplyFrail Int
  | GainBlock Int
  | GainStrength Int
  deriving (Show, Eq)

data Relics = Relics
  { _suriken :: Int
  , _kunai   :: Int
  }

data Logger = Logger
  { _history     :: String
  , _cardsPlayed :: Sum Int
  }

newtype Sum a = Sum a
  deriving (Show, Eq)

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Semigroup Logger where
  Logger c1 l1 <> Logger c2 l2 =
    Logger (c1 `mappend` c2) (l1 `mappend` l2)

instance Monoid Logger where
  mempty = Logger mempty mempty

-------------------------------------------------------------------------------

strike, defend, bash :: Card
strike = Card Strike Attack 1 "Deal 6 damage."
defend = Card Defend Skill  1 "Gain 5 blocks."
bash   = Card Bash   Attack 2 "Deal 8 damage. Apply 2 vulnerable."

ironclad, cultist, jawWorm :: Unit
ironclad = Unit Ironclad 80 80 0 0 0 0 0 0 (const [NoIntend])
cultist  = Unit JawWorm  40 40 0 0 0 0 0 0 (const [DealDamage 6, GainStrength 3])
jawWorm  = Unit Cultist  48 48 0 0 0 0 0 0
  (\t -> let r = mod t 3 in
    if r == 0 then [DealDamage 11]
    else if r == 1 then [DealDamage 7, GainBlock 5]
    else [GainStrength 3, GainBlock 9]
  )
shieldGremlin = Unit ShieldGremlin 13 13 0 0 0 0 0 0 (const [GainBlock 7, ApplyVulnerable 1])
madGremlin = Unit MadGremlin 20 20 0 0 0 0 0 0 (const [DealDamage 7])
fatGremlin = Unit FatGremlin 15 15 0 0 0 0 0 0 (\t -> if even t then [DealDamage 5, ApplyWeak 1] else [DealDamage 4, ApplyFrail 1])
sneakyGremlin = Unit SneakyGremlin 10 10 0 0 0 0 0 0 (const [DealDamage 10])
gremlinWizard = Unit GremlinWizard 23 23 0 0 0 0 0 0 (\t -> if t == 3 then [DealDamage 25] else [NoIntend])

initialBattle :: Battle
initialBattle = Battle
  { _player  = ironclad
  , _enemies = [madGremlin]
  , _mana = 0
  , _turn = 0
  , _cardPool = CardPool
    (replicate 5 strike ++ replicate 5 defend ++ [bash])
    []
    []
  }
