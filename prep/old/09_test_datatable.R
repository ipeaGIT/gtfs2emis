DT = data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
DT[, .SD[1]] # first row of all columns
DT[, .SD[1], by=x] # first row of 'y' and 'v' for each group in 'x'
DT[, c(.N, lapply(.SD, sum)), by=x] # get rows *and* sum columns 'v' and 'y' by group
DT[, .I[2], by=x] # row number in DT corresponding to each group

#DT[,.SD[1],by=x]
DT
DT[,emi:=sum(y),by=x][,.SD[1],by=x]
setDT[]

DT = data.table(a = LETTERS[c(3L,1:3)], b = 4:7)
DT[, c := 8] # add a numeric column, 8 for all rows
DT[, d := 9L] # add an integer column, 9L for all rows
DT[, c := NULL] # remove column c
DT[2, d := -8L] # subassign by reference to d; 2nd row is -8L now
DT # DT changed by reference
DT[2, d := 10L][] # shorthand for update and print
DT[b > 4, b := d * 2L] # subassign to b with d*2L on those rows where b > 4 is TRUE
DT[b > 4][, b := d * 2L] # different from above. [, := ] is performed on the subset
# which is an new (ephemeral) data.table. Result needs to be
# assigned to a variable (using `<-`).
DT[, e := mean(d), by = a] # add new column by group by reference
DT["A", b := 0L, on = "a"] # ad-hoc update of column b for group "A" using
# joins-as-subsets with binary search and 'on='
# same as above but using keys
setkey(DT, a)