module Grammar where

data Element = Node NodeType String [Field] [Method]
             | Edge EdgeEnd EdgeEnd EdgeStyle EdgeAnnotation
             deriving (Eq, Show)

data NodeType = Class | Interface | AbstractClass | Enum deriving (Eq, Show)
data NodeElement = NodeElement String | NodeBreak deriving (Eq, Show)
type Field = NodeElement
type Method = NodeElement

data EdgeEnd = EdgeEnd String EdgeHead deriving (Eq, Show)
data EdgeStyle = Solid | Dotted deriving (Eq, Show)
type EdgeHead = String
type EdgeAnnotation = String
