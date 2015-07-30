namespace Types

    open System

    type TimeUnit = DateTime
    type Id = uint64
    type Trail = Id list

    type Origin = | User = 1 | Derived = 2 | ParentModule = 3 | PeerModule = 4 | ChildModule = 5
    type Tense = | Eternal | Past | Present | Future
    type Truth = {F : float32; C : float32}
    
    and TermType = | Inh | Sim | Imp | Equ | PreImp | RetImp | ConImp | ConEqu | PreEqu | TermLink
                   | ExtSet | IntSet | ExtInt | IntInt | ExtDif | IntDif
                   | Prod | ExtImg | IntImg
                   | And | Or | Not
                   | Par | Seq | Operator
                   | QVar | IVar | DVar

    and Term =  | Term of TermType * Term list
                | Constant of string

    type Desire     = Truth
    type Priority   = float32 
    type Stamp      = {Created : int64; Occurs : Tense; SC : int; Origin : Origin; mutable Trail : Trail}
    type Judgement  = {Term : Term; TV :  Truth}
    type Question   = {Term : Term}
    type Quest      = {Term : Term}
    type Goal       = {Term : Term; DV : Desire}
    type Sentence   = | Judgement of Judgement
                      | Question of Question
                      | Quest of Quest
                      | Goal of Goal

    type Task = {mutable P : Priority; S : Sentence; Stamp : Stamp}
    type Belief = {J : Judgement; Stamp : Stamp}

