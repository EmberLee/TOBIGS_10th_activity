<img width="720" alt="default" src="https://user-images.githubusercontent.com/35494111/44088430-0836a0fc-9ffe-11e8-9c9c-d65a9909b285.PNG">

```Python
##### With Python #####
inp = [[7],[3,8],[8,1,0],[2,7,4,4],[4,5,2,6,5]]
# 예제 정수삼각형
inp2 = [[6],[3,5],[1,1,0],[2,7,4,4],[4,4,8,10,1],[10,3,2,9,4,12]]
len(inp) # 삼각형 층수 확인

# 아래층에 있는 숫자는 위층에서 누적된 최대합(왼쪽, 오른쪽 중 하나)을 선택해 자기자신과 더하면 된다. 
# 만일 삼각형의 양변 끝에 있는 숫자들이라면 바로 윗층 숫자에 그대로 더해진다.
# 따라서 각 층 숫자위치별로 최대합이 기록되어 있는 sum삼각형을 새로 작성한다.

def triangle(x):
    memo = []
    memo.append(x[0]) # 1층은 숫자 하나이므로 그냥 추가
    for i in range(1, len(x)):
        memo.append([0 for _ in range(i+1)]) # 메모장의 i층을 초기화
        
        for j in range(len(x[i])): # i번째 층 탐색
            if j == 0:
                memo[i][j] = memo[i-1][j] + x[i][j] 
            # 메모리 테이블의 각 층 왼쪽, 오른쪽 끝은 윗층의 
            # 왼쪽, 오른쪽 끝으로부터 내려올 수밖에 없으므로 단순 합산하면 된다.
            elif j == len(x[i])-1:
                memo[i][j] = memo[i-1][j-1] + x[i][j]
            else:
                memo[i][j] = max(memo[i-1][j-1], memo[i-1][j]) + x[i][j]

    return memo, max(memo[len(x)-1]) # 맨 아래층 메모장에 적힌 경로 합산값들 중 최대값 하나를 산출
    
triangle(inp2)
```

<img width="720" alt="default" src="https://user-images.githubusercontent.com/35494111/44088456-1bb4f30e-9ffe-11e8-812e-401abf6efe05.PNG">

```R
##### With R #####
# 메모장은 인풋 매트릭스와 같은 차원의 행렬이며, 
# 각 행.열에 메모하는 정보는 현재 행.열까지 만들어진 정사각형의 한 변의 길이이다.
# |-> 추가설명 : memo[i, j] = 오리지널 행렬의 i, j 성분을 
# 정사각형의 오른쪽 아래 꼭지점 끝으로 삼는 가장 큰 정사각형의 한 변의 길이

# 인풋 매트릭스의 원소가 0일 경우 해당 행.열의 메모장 정보는 즉시 0이 되며, (정사각형을 형성하지 못하므로)
# 메모장 행.열의 바로 왼쪽, 위, 대각선 왼쪽 위가 모두 정사각형이 형성되어 있으면(1 이상) 
# 현재 행.열은 여지껏 형성된 정사각형들 중 최소 변의 길이를 갖는 정사각형의 변+1이다.

Large <- function(mat){
  memo <- matrix(0, nrow(mat), ncol(mat))
  memo[1, ] <- mat[1, ]
  memo[, 1] <- mat[, 1]
  for (i in 2:nrow(mat)){
    for (j in 2:ncol(mat)){
      if (mat[i, j] == 1){
        memo[i, j] <- min(memo[i-1, j], memo[i, j-1], memo[i-1, j-1])+1
      } else{
        memo[i, j] <- 0
      }
    }
  }
  return((max(memo))^2)
}

Mat1 <- matrix(c(1, 0, 1, 1, 1,
                 0, 0, 0, 1, 1,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1,
                 0, 1, 1, 1, 1),5, 5, byrow=TRUE)
Mat2 <- matrix(c(1, 0, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1,
                 1, 0, 1, 1, 1, 1, 1,
                 0, 1, 1, 1, 1, 1, 1,
                 0, 0, 1, 1, 1, 1, 1), 5, 7, byrow=TRUE)

Mat3 <- matrix(c(1, 0, 0, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1,
                 0, 1, 1, 1, 1, 1, 1, 1,
                 0, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1 ), 6, 8, byrow=TRUE)

Large(Mat1); Large(Mat2); Large(Mat3)
```

<img width="711" alt="default" src="https://user-images.githubusercontent.com/35494111/44088608-c46d3a6a-9ffe-11e8-854f-ca3da1f710d4.PNG">

```Python
##### With Python #####
inp = [10, 20, 40, 25, 30, 15, 50]
# LIS = [10, 20, 25, 30, 50]

# O(N^2) 시간복잡도가 걸리는 방법
# 단순하고 생각해내기 쉬움
# memo[i] = 오리지널 리스트 1~i번째 성분까지를 인풋으로 생각했을 때 
# Longest Increasing Subsequence(LIS)의 길이
# 오리지널 리스트의 i+1번째 성분을 추가해 고려했을 때 기존 LIS 길이(=memo[j])보다
# 긴 LIS를 만들 수 있을 때 memo[i] = memo[j] + 1을 수행한다.
def partial_longest(seq):
    memo = [1 for _ in range(len(seq))]
    n = len(seq)
    for i in range(1, n):
        for j in range(i):
            if seq[i] > seq[j] and memo[i] < memo[j] +1:
                memo[i] = memo[j] + 1
    return max(memo) 
    
partial_longest(inp)
partial_longest([10, 22, 9, 20, 21, 50, 41, 45, 90])
```

<img width="728" alt="default" src="https://user-images.githubusercontent.com/35494111/44088673-0473fb80-9fff-11e8-81f9-8cf5dea035fe.PNG">

```R
##### With R #####
# A(10, 20), B(20, 5), C(5, 30), D(30, 15)
# input : c(10, 20, 5, 30, 15)
# 이 문제는 matrix chain multiplication으로 DP issue 중 하나이다.
# memo[i, j] = i번째 젓가락쌍부터 j번째 젓가락쌍까지만을 가지고 계산한 최소배송비
# ex)memo[1, 3] = A, B, C를 가지고 계산한 최소배송비 -> 이 때 임시 인풋은 c(10, 20, 5, 30)
# 그렇다면 A(B, C, D) = memo[1, 1] + memo[2, 4] + input[1]*input[2]*[5]가 되고
# (A, B)(C, D) = memo[1, 2] + memo[3, 4] + input[1]*input[3]*input[5]가 된다.
# 이런 식으로 메모리 테이블을 채워나간다. 메모리 테이블이 행렬이지만 매우 작은 시간복잡도로 계산해낼 수 있다.

chopchop <- function(x){
  X <- x
  N <- length(x) - 1
  memo <- matrix(0, N, N) # memo[i, i] = 0이다. 왜냐하면 단 한 쌍의 젓가락쌍만으로는 배송을 할 수 없기 때문
  
  for (s in 2:N){
    for (i in 1:(N-s+1)){ 
      j <- i+s-1
      memo[i, j] <- Inf # min함수에서의 비교를 위해 초기화
      for (k in i:(j-1)){
        memo[i, j] <- min(memo[i, j], memo[i, k] + 
                            memo[k+1, j] +
                            X[i]*X[k+1]*X[j+1])
      }
    }
  }
  return(memo[1, N])
}

test1 <- c(10, 20, 5, 30, 15)
test2 <- c(10, 20, 5, 30)
test3 <- c(30, 35, 15, 5, 10, 20, 25)
chopchop(test1); chopchop(test2); chopchop(test3)
```

<img width="720" alt="default" src="https://user-images.githubusercontent.com/35494111/44088729-3002baf2-9fff-11e8-973e-d45a9ada36dc.PNG">

```Python
##### With Python #####
test = [1, 2, 3]
m = len(test)

# Recursion으로 풀기
# input : S, m, n -> S:[금액 단위 종류들 ex)10, 100, 500], m:S의 길이, n:지불할 금액
# 1000원을 10, 100, 500원의 금액 단위로 지불하고자 한다면 경우의 수는 두 가지로 나뉜다.
# 1. 500원 한 개를 반드시 포함한 채로 나머지(1000-500 = 500)를 지불한다.
# 2. 500원이라는 금액 단위를 제외하고 나머지 10, 100원만 가지고 1000원을 지불한다.
# 이상의 방법으로 재귀식을 세운다.
def count2(S, m, n):
 
    # If n is 0 then there is 1
    # solution (do not include any coin)
    if n == 0:
        return 1
 
    # If n is less than 0 then 
    # no solution exists
    if n < 0:
        return 0;
 
    # If there are no coins and n
    # is greater than 0, then 
    # no solution exist
    if m <= 0 and n >= 1:
        return 0
 
    # count is sum of solutions (i) 
    # including S[m-1] (ii) excluding S[m-1]
    return count2( S, m - 1, n ) + count2( S, m, n-S[m-1] )
    
# Memoization으로 풀기
def count1(S, m, n):
    # We need n+1 rows as the table is constructed 
    # in bottom up manner using the base case 0 value
    # case (n = 0)
    memo = [[0 for x in range(m)] for x in range(n+1)]
 
    # Fill the entries for 0 value case (n = 0)
    for i in range(m):
        memo[0][i] = 1
 
    # Fill rest of the table entries in bottom up manner
    for i in range(1, n+1):
        for j in range(m):
 
            # Count of solutions including S[j]
            x = memo[i - S[j]][j] if i-S[j] >= 0 else 0
 
            # Count of solutions excluding S[j]
            y = memo[i][j-1] if j >= 1 else 0
 
            # total count
            memo[i][j] = x + y
 
    return memo[n][m-1]
    
print(count1(test, m, 4))
print(count2(test, m, 4))
```
