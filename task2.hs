
-- nlcc function takes image, value as input and return the max connected pixels for a given value.
nlcc :: [[Int]] -> Int -> Int
nlcc image value = do

    -- row variable contains the number of rows present in the image/matrix.
    let row = length image 
    -- column variable contains number of columns present in image/matrix.
    let column = length (image !! 0)

    -- we traverse through each and every cell(row,column) and stores largest number of connected pixels. 'l' is a list which contains all the connected pixels in that row and column.
    --      Example : l = [0,0,0,19,19,..........,0] 
    let l = [first (traverseMatrix image i j value) | i<-[0..row],j<-[0..column]]

    -- returns max connected pixels
    maximum l


-- first function is used to return first value present in tuple. Example: first (1,2) => 1
first :: (a, b) -> a
first (x,_) = x



-- traverseMatrix is a recursion function that helps us to traverse in 4 directions(top,bottom,left,right) of the image. It returns the a tuple which contains conneted pixels and updated image.

traverseMatrix :: [[Int]] -> Int -> Int -> Int -> (Int, [[Int]])
traverseMatrix image row column value =  do

    -- isValidMove is the base function that helps to validate whether we can traverse through a given row and column.
    --          Example: If given row or column index is greather than matrix/image size so, in that case isValidMove fucntion returns False.
    if (isValidMove image row column value) == False 
        then do
            (0,image)
    else do
        -- 
    --If the value of a particular cell image[row][column] is counted, we update that value to -1 to avoid repetitions.
    --          Example: imgage[1][1] = -1,
        let updated_image = updateImage image row column

        -- Below line traverse through right direction. returns => (right connected pixels, updated image)
        let (right, updated_image_right) = traverseMatrix updated_image row (column+1) value
        -- Below line traverse through left direction. returns => (left connected pixels, updated image)
        let (left, updated_image_left) = traverseMatrix updated_image_right row (column-1) value
        -- Below line traverse through bottom direction. returns => (bottom connected pixels, updated image)
        let (bottom, updated_image_bottom) = traverseMatrix updated_image_left (row+1) column value
        -- below line traverse through top direction. returns => (top connected pixels, updated image)
        let (top, updated_image_top) = traverseMatrix updated_image_bottom (row-1) column value

        -- Adding all the connected pixels in 4 directions.
        (1+right+left+bottom+top,updated_image_top)


-- isValidMove accepts image, row, column value as input and checks if we can navigate through row and column index.
isValidMove :: [[Int]] -> Int -> Int -> Int -> Bool
isValidMove image row column value = do
    -- condition 1: we cannot travese if row and column index is less than 0
    -- condition 2: we cannot travese if row and column index is greater than matrix/image row length and column length
    -- condition 3: if image[row][column] value is not equal to given value we should not traverse.
    -- condition 4: if image[row][column] value was already counted/visited we  should traverse through that cell. If the cell is visited then we mark it as -1.
    if row < 0 || column < 0 || row > (length image)-1  || column > (length (image !! 0)-1) || (getValue image row column == -1) || (getValue image row column /= value) 
        then 
            False
        else 
            True



-- getValue fucntion takes matix row, column as input parameters and return the value present in that row, column. If row, column index are greater than matrix/image size it returns -1.
--                  Example: getValue [[0,1],[1,100]] 1 1 => 100
--                  Example: getValue [[0,1],[1,0]]  3 3 => -1 
getValue :: [[Int]] -> Int -> Int -> Int
getValue image row column = do 
    if (row < 0 || column < 0 || row > (length image)-1 || column > (length (image !! 0)-1)) 
        then -1 
    else 
        (image !! row) !! column 


-- updateImage takes matrix/image row column as input and update image[row][column] to -1. 
--                  Example: updateImage [[1,0,1],[0,0,0]] 1 1 => [[1,0,1],[0,-1,0]]
updateImage :: [[Int]] -> Int -> Int -> [[Int]]
updateImage image r c = map (\(rowIndex,row) -> if rowIndex == r then updateList row c   else row) (zip [0..] image)

-- updateList takes list, index as input and update list[index] with -1. we use this funtion to updateImage
--                  Example: updateList [1,2,3] 1 => [1,-1,3]
updateList :: [Int] -> Int -> [Int]
updateList l index  =  map (\( i , val ) -> if i == index then -1 else val ) (zip [0..] l)
