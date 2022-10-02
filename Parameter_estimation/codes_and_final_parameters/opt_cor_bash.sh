
for i in {1..12}; do mkdir $i ; done
function recursive_for_loop { 
    ls -1| until ! read f; do
        if [ -d $f  -a ! -h $f ];  
        then  
            cd -- "$f";  
             echo "Doing something in folder `pwd`/$f"; 
             #rm *.f90

              
             
             #cp ../constants.f90 .
             cp ../random_interaction_matrix.f90 .
            sed  "s/p1=/p1=$f/g" random_interaction_matrix.f90 > random_interaction_matrix_new.f90
            sed  "s/seed=/seed=$f+/g" random_interaction_matrix_new.f90 > random_interaction_matrix_new1.f90
            # sed  "s/sc=/sc=$f/g" constants.f90 > constants_new.f90
             
              
              gfortran random_interaction_matrix_new1.f90 -o bs4
              ./bs4
             #rm -r analysis/ 
              cp ../*.f90 .
              cp ../*.dat .
#              gfortran constants_new.f90
               gfortran constants.f90 mc17_new.f90 -o out_$f
               nohup ./out_$f &    

            # ./a.out
              
             #result=${PWD##*/}          # to assign to a variable
            # printf 'result'
            ##  tail -1 pop.txt > pop_ec.dat
             # tail -1 pop.txt > pop.dat
              
                 
              
             # cp pop_ec.dat ../../analysis/$f
              
            #  cp pop.dat ../../hp/$f

             # cp pop.dat ../../hc/$f

             # cp pop.dat ../../hc/$f
             # cp pop.dat ../../hc/$f
             # printf '%s\n' "${PWD##*/}"
              #printf  "${PWD##*/}"

             
             # rm -r analysis/
            # use recursion to navigate the entire tree
            recursive_for_loop;
            cd ..; 
        fi;  
    done; 
};
recursive_for_loop
