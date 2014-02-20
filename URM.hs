module URM where
type NN=Integer
data Instruction = Z NN|S NN|T NN NN|J NN NN NN

