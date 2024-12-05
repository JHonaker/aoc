len = 0 are all safe
len = 1 are all safe
len = 2
    safe if diff \in [1,3]

len = 3

For any safe run of length point i, that is D=increasing,
    a run of i+1 is safe if 
        abs(diff) \in [1,3]
        diff > 0 

Arr[i:j] is +safe if Arr[i:k] is +safe and Arr[k:j] is +safe
    or for one i<=n<=j
        Arr[i:n-1] is +safe, Arr[n+1:j] is +safe, and Arr[n-1] to Arr[n+1] is a +safe jump

Arr[i:i+1] and Arr[i:i] is safe for all i
