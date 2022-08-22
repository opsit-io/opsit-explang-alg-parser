### implement quicksort in explang

function swap(a, i, j) 
    "swap two elements in an indexable sequence";
    tmp:= get(a, i);
    aset(a, i, get(a,j));
    aset(a, j, tmp);
end;


function random_list(size, maxr)
    "make list of length size of random number from 0 upto maxr";
    map( function (x)
            random(maxr);
         end,
         range(0, size));
end;

function partition(a, lo, hi)
    "partition part of an indexable sequence a";
    pivot := get(a, hi);
    i := lo;
    j := lo;
    while( (j < hi),
           begin
               if (get(a, j) < pivot) 
                   swap(a, i, j);
                   i := i + 1;
               end;
               j := j + 1;
           end);
    swap(a, i, hi);
    i;
end;

function quick_sort (a, lo, hi)    
    "quick sort an indexable sequence or part of it if lo and hi parameters are set";
    if (lo < hi)
        p := partition(a, lo, hi);
        quick_sort(a, lo,    p - 1);
        quick_sort(a, p + 1, hi);
    end;
    a;
end;

samples := list( random_list(32, 100), # list of random integers
                 append(make_array(0), random_list(32,100)),
                 append(list(), "Hello people! How do you do?"),
                 list(v"0.1.2",  v"0.1.2-pre1", v"0.1.2-pre2", v"0.1.3", v"0.1.2-pre1+10", v"1.2.3"));
map( function(data)
        print(str("Using quick sort on:  ", type_of(data), "\n"));	
        print(str("input=", data, "\n"));
        quick_sort(data, 0, length(data) - 1);
        print(str("output=",data, "\n\n"));
     end, samples);
           
                 
# (let ((samples (list (random-list 32 100) ; list of random integers
# 		     ;; array of random integers
# 		     (append (make-array 0) (random-list 32 100)) 
# 		     ;; StringBuffer
# 		     (.N "StringBuffer" '("Hello people! How do you do?")) 
# 		     ;; list of Characters
                 
# 		     ;; semantic versions
# 		     (map #'version '("0.1.2" "0.1.2-pre1" "0.1.2-pre2" "0.1.3" "0.1.2-pre1+10" "1.2.3")))))   
#   (map (lambda (sample)
# 	 (print (str "Using quick sort on:  " (type-of sample) "\n"))	 
# 	 (print (str "input:  " sample "\n"))
# 	 (quick-sort sample)
# 	 (print (str "output: " sample "\n\n")))
#        samples))

#data:=[10, 2, 9, 0];
#data := random_list(10,100); 
#print(str("input=", data, "\n"));
#quick_sort(data, 0, length(data) - 1);
#print(str("output=",data, "\n"));


