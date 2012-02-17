P=[2]
for x in range(3,1010):
     count=0
     for p in P:
             count=count+1
             if x%p==0:
                     break
             else:
                     if len(P)==count:
                             P.append(x)
                     continue
