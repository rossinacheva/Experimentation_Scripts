

darts_seq  = c(20,1,18,4,13,6,10,15,2,17,3,19,7,16,8,11,14,9,12,5) # sequence in darts
allowed_gap = 5

e <- 2
while (e > 0) {

a = round(runif(6, min=1, max=20),0) #generates random nubbers between 1 and 20
b = match(a,darts_seq) #extracts the positions of the random numbers on the darts table
c =  b - c(NA, b[1:(length(b)-1)]) #calculates the difference between the positions
d = ifelse(abs(c) < allowed_gap,1,0) # checks if we have proper gaps between the numbers
f = sum(d[2:6])  # sums the checks on differences. 0 if all ok
g = ifelse(length(unique(a))==6,0,1)
e = f + g
}
print(a)