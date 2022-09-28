        program main 
 
           use constants
        implicit none 
          

        real,dimension(tnob,tnon)::ds,pb,pb1,cb,cb1,fb,fb1,fib,fib1,ph,ch,fh,fih,pt,ct,ft,fit
        real,dimension(tnob,tnon) ::growth_p,growth_c,growth_f,growth_fi
        real,dimension(tnob,tnon) ::growth_pn,growth_cn,growth_fn,growth_fin
        real, dimension(tnob,tnob) :: bs,ec,bs_new,bs1,corr_bu,corr_au,bs2
        real,dimension(tnob,sp)::b,b1,b2,b_new,br,br_new,br_newn,b_newn,b_com1,b_com2,b_new1
        integer :: i, j,ss,ts,xp1,xm1,un,m,um,q,mm
        real, dimension(sp) :: x, c_p,c_f,c_c,c_fi,rn,ln,c_p_new,c_f_new,c_c_new,c_fi_new,&
         & c_p_newn, c_f_newn,c_c_newn,c_fi_newn
        real ::asso,p_con,c_con,f_con,fi_con,a,asso1,asso2,asso3,asso4
        real,dimension(tnob)::alpha_p,alpha_p1,alpha_c,alpha_c1,alpha_f,alpha_f1,alpha_fi,alpha_fi1,deathb,growth,growthf,k0
        real,dimension(tnob)::alpha_pn,alpha_pn1,alpha_cn,alpha_cn1,alpha_fn,alpha_fn1,&
        &alpha_fin,alpha_fin1,deathbn,growthn,growthfn,re,re1
        real :: bsum,t1,rmsd1,rmsd2,rmsd3,rmsd4
        real :: ri3,rmsd
        integer :: ri1,ri2,ri4,ri5
        real ::rr0, rrr0, rr1 , rr2, rr3,rr4,rr5,rr6,re2,re3,btav,ree
        integer :: cim, acim,d,tss,awm
         
        real, dimension(tnob,tons) ::  b_av
        integer :: u
        real :: tot,startTime, stopTime
        

    !    cim=0
    !    btav=0
    !    acim=0
    !    un=7   ! Initial value is 7 is because 6 is used to write on screen 
    !    um=7
        cfl=c*(dt/dx)
        open(201, file='bd.dat',action='read',status='old')
        !open(202, file='bs4.dat',action='read',status='old')
        open(200,file='av_bi.dat',action='read',status ='old')
       
        open(1, file='hi.txt',STATUS='NEW')     
        open(203, file='rmsd.txt',action='write',STATUS='NEW')     
!        open(37, file='matrix.txt',STATUS='NEW')     
        open(180, file='pop.txt',STATUS='NEW',action='write',position='append')
        open(250, file='r_error.txt',STATUS='NEW',action='write',position='append')
        open(455, file='matrix_element.txt',status='new',action='write',position='append')
        open(555,file='time.txt',status='new',action='write',position='append')
          

do i = 1, tnob
        read(201,*) (ds(i,j), j=1, tnon)

     end do
        close(201)
    ! do i = 1, tnob

    !    read(202,*) (bs(i,j), j=1, tnob)

    ! end do

    !    close(202)



do u=1,tons
  
        cim=0
        btav=0
        acim=0
        un=7   ! Initial value is 7 is because 6 is used to write on screen 
        um=7
        awm=0


        
          open(202, file='bs4.dat',action='read',status='old')



          do i = 1, tnob

               read(202,*) (bs(i,j), j=1, tnob)

          end do

          close(202)
         


 
         !filename="matrix"//trim(ab)//"np_"//trim(abc)//".txt"
         write(ab,'(I0)') u
         filename="matrix_"//trim(ab)//".txt"
        ! filename1="matrix_element_"//trim(ab)//".txt"
         un=un+1
        ! um=um+1
        ! open(unit=um,file=filename1)
         open(unit=un,file=filename)

    read(200,*)(b_av(i,u),i=1,tnob)
      
    write(*,*)(b_av(i,u),i=1,tnob)
    do ss=1,sp
       do i=1,tnob
         b(i,ss)=(bt/tnob)
         b1(i,ss)=(bt/tnob)
       end do
      end do


   do i=1,tnob
       btav=btav+b_av(i,u)
    end do

              do ss=1,sp
                x(ss)=ss*dx
           
                C_P(ss)=p0    !Concentration of Cnutrient in mili molar
                C_F(ss)=f0    !Concentration of nutrient Cin mili molar
                C_C(ss)=c0    !Concentration of nutrient Cin mili molar
                C_fi(ss)=fi0
              end do           

!do m=1,1000
!write(6,*) m
rr3=1.0

!open(19, file='pop_bu.txt',STATUS='NEW',action='write',position='append')


ts1loop:  do ts=1,tp

        if(mod(ts,21600) .eq. 0) then
     
               do ss=1,sp
                                    
                C_P(ss)=p0    !Concentration of Cnutrient in mili molar
                C_F(ss)=f0    !Concentration of nutrient Cin mili molar
                C_C(ss)=c0    !Concentration of nutrient Cin mili molar
                C_fi(ss)=fi0

               end do
           

         ELSE


                           do ss=1,sp

                C_P(ss)=C_P(SS)    !Concentration of Cnutrient in mili molar
                C_F(ss)=C_F(SS)    !Concentration of nutrient Cin mili molar
                C_C(ss)=C_C(SS)    !Concentration of nutrient Cin mili molar
                C_fi(ss)=C_fi(SS)

               end do
       
        end if 
          


    do ss=1,sp

        bsum=0 
        do i=1,tnob
        bsum=bsum+b(i,ss)
       ! b_new(i,ss)=0
        end do

    
       do i=1,tnob
          !do =l,tnon
          pb(i,1) = delta*(c_p(ss)/(k_s+c_p(ss)))*(1.0/(exp(-ds(i,1)+1.0)))
          cb(i,2) = delta*(c_c(ss)/(k_s+c_c(ss)))*(1.0/(exp(-ds(i,2)+1.0)))
          fb(i,3) = delta*(c_f(ss)/(k_s+c_f(ss)))*(1.0/(exp(-ds(i,3)+1.0)))
          fib(i,4) =delta*(c_fi(ss)/(k_s+c_fi(ss)))*(1.0/(exp(-ds(i,4)+1.0)))
          


          pb1(i,1) = delta*(c_p(ss)/(k_s+c_p(ss)))
          cb1(i,2) = delta*(c_c(ss)/(k_s+c_c(ss)))
          fb1(i,3) = delta*(c_f(ss)/(k_s+c_f(ss)))
          fib1(i,4) =delta*(c_fi(ss)/(k_s+c_fi(ss)))




          alpha_p(i)=pb(i,1)/delta1
          alpha_c(i)=cb(i,2)/delta1
          alpha_f(i)=fb(i,3)/delta1
          alpha_fi(i)=fib(i,4)/delta1

          alpha_p1(i)=pb1(i,1)/delta1
          alpha_c1(i)=cb1(i,2)/delta1
          alpha_f1(i)=fb1(i,3)/delta1
          alpha_fi1(i)=fib1(i,4)/delta1

          
   
         
          ph(i,1) =delta2*(c_p(ss)/(k_s+c_p(ss)))*(1.0/(exp(-ds(i,1)+1.0)))
          ch(i,2) =delta2*(c_c(ss)/(k_s+c_c(ss)))*(1.0/(exp(-ds(i,2)+1.0)))
          fh(i,3) =delta2*(c_f(ss)/(k_s+c_f(ss)))*(1.0/(exp(-ds(i,3)+1.0)))
          fih(i,4)=delta2*(c_fi(ss)/(k_s+c_fi(ss)))*(1.0/(exp(-ds(i,4)+1.0)))

          pt(i,1)  =   pb(i,1)+ph(i,1)
          ct(i,2)  =   cb(i,2)+ch(i,2)
          ft(i,3)  =   fb(i,3)+fh(i,3)
          fit(i,4) =   fib(i,4)+fih(i,4)
         ! end do
          asso=0
          asso1=0
          asso2=0
     
               do j=1,tnob
                    if (bs(i,j) < 0.0) then
                       asso1=asso1+bs(i,j)*b(j,ss)
                    end if
    
                    if (bs(i,j) > 0.0 ) then
                       asso2=asso2+bs(i,j)*b(j,ss)
                    end if         
               end do
          
          growth_p(i,1)=alpha_p(i)*dt*b(i,ss)+&
           &alpha_p1(i)*dt*b(i,ss)*(-sc*b(i,ss)+asso1+asso2)/k
          growth_c(i,2)=alpha_c(i)*dt*b(i,ss)+&
           &alpha_c1(i)*dt*b(i,ss)*(-sc*b(i,ss)+asso1+asso2)/k
          growth_f(i,3)=alpha_f(i)*dt*b(i,ss)+&
           &alpha_f1(i)*dt*b(i,ss)*(-sc*b(i,ss)+asso1+asso2)/k
          growth_fi(i,4)=alpha_fi(i)*dt*b(i,ss)+&
           &alpha_fi1(i)*dt*b(i,ss)*(-sc*b(i,ss)+asso1+asso2)/k
         
         ! growth_p(i,1)=alpha_p(i)*dt*b(i,ss)*(1+(-sc*b(i,ss)+asso1+asso2)/k)
         ! growth_c(i,2)=alpha_c(i)*dt*b(i,ss)*(1+(-sc*b(i,ss)+asso1+asso2)/k)
         ! growth_f(i,3)=alpha_f(i)*dt*b(i,ss)*(1+(-sc*b(i,ss)+asso1+asso2)/k)
         ! growth_fi(i,4)=alpha_fi(i)*dt*b(i,ss)*(1+(-sc*b(i,ss)+asso1+asso2)/k)
  
           deathb(i) = death*dt*(b(i,ss))
           growth(i) = growth_p(i,1)+growth_c(i,2)+growth_f(i,3)+growth_fi(i,4)
        
           growthf(i)=growth(i)
           b_new(i,SS)=b(i,ss)+growthf(i)-deathb(i)
       
      end do



            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         

                    
    d=mod(ts,ts1)
         if (d .eq. 0) then

               ! do ss=1,sp

               !     write(180,*)ts,(b(i,ss), i =1,tnob)
         !           write(6,*)ts,(b(i,ss), i =1,tnob)
               ! end do
        ! end if



 
                        rr0=0.0
                              do i=1,tnob
                    
                                rr0=rr0+((b_new(i,ss)-b1(i,ss)))**2
                               ! write(*,*)ts,b_new(i,ss),b1(i,ss),sqrt(rr0)/float(tnob)
                    
                              end do
                               !write(*,*)  rr0 
                                rrr0=sqrt(rr0)/float(tnob)
                            
                               ! write(*,*)ts,rrr0 
                               if(rrr0 .lt. abs(1000000.0)) then        

!                   write(*,*)ts, rrr0
        
            
                                   exit ts1loop
                     
                              else

                   


              
                                     do i=1,tnob

                                         b1(i,ss)=b_new(i,ss)
                                    end do
              
                            end if
           
             

         end if     



                            p_con=0.0
                            c_con=0.0
                            f_con=0.0
                            fi_con=0.0
                
                       do i=1,tnob
                            p_con=p_con+(b(i,ss))*pt(i,1)
                            c_con=c_con+(b(i,ss))*ct(i,2)
                            f_con=f_con+(b(i,ss))*ft(i,3)
                            fi_con=fi_con+(b(i,ss))*fit(i,4)
                       end do
                
                            C_P_new(ss)=C_P(ss)-p_con
                            C_c_new(ss)=C_c(ss)-c_con
                            C_f_new(ss)=c_f(ss)-f_con
                            C_fi_new(ss)=c_fi(ss)-fi_con
                
               end do
                
                
                    do ss=1,sp   
                       C_P(ss)=c_P_new(ss)
                        C_C(ss)=c_C_new(ss)
                        C_F(ss)=c_F_new(ss)
                       C_Fi(ss)=c_Fi_new(ss)
                      end do

         ! write(*,*) 'hi'
              do ss=1,sp
               do i=1,tnob

                                         b(i,ss)=b_new(i,ss)
                                    end do
              end do

!end if

          d=mod(ts,ts1)
         if (d .eq. 0) then

                do ss=1,sp

                    write(180,*)ts,(b(i,ss), i =1,tnob)
         !           write(6,*)ts,(b(i,ss), i =1,tnob)
                end do
         end if





end do ts1loop



   do ss=1,sp

      do i=1,tnob

         b2(i,ss)=b(i,ss)
         !b_new1(i,ss)=b(i,ss)
      end do
  
  end do








rmsd1=0.0
do ss=1,sp
 do i=1,tnob
     

      rmsd1=rmsd1+((b(i,ss)-b_av(i,u))/b_av(i,u))**2

 

end do
    end do

      rmsd1=sqrt(rmsd1/float(tnob))


do m=1, tnos


!write(6,*) m


call cpu_time(startTime)


     do i=1,tnob
        do j=1,tnob
           bs2(i,j)=bs(i,j)
        end do
    end do


         ri1=int((ub-lb+1)*rand()+lb)
         ri2=int((ub-lb+1)*rand()+lb)
         ri4=int((ub-lb+1)*rand()+lb)
         ri5=int((ub-lb+1)*rand()+lb)
         ri3=int((ub1-lb1+1)*rand()+lb1)-2


!write(*,*) ri4,ri5

        
                           bs1(ri4,ri5)=bs2(ri4,ri5)+((rand()-0.5)*ctm)
                             do i=1,tnob
                             
                                do j=1, tnob

                                 if ((i .eq. ri4) .and. (j .eq. ri5) ) then
                                      
                                     bs_new(i,j)=bs1(ri4,ri5)
                                                                          
!
                                 else      ! this is not working
                                
                                     bs_new(i,j)=bs2(i,j)
                                           
                                 end if  
                             
                                end do
                          
                            end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!Calculation with updated matrix!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 do ss=1,sp

                C_P_new(ss)=p0    !Concentration of Cnutrient in mili molar
                C_F_new(ss)=f0    !Concentration of nutrient Cin mili molar
                C_C_new(ss)=c0    !Concentration of nutrient Cin mili molar
                C_fi_new(ss)=fi0
              end do




   do ss=1,sp
        do i=1,tnob
         b_new1(i,ss)=b(i,ss)
         !b2(i,ss)=b(i,ss)
        end do
    end do


ts2loop:           do ts=1,tp
!write(6,*) 'hi'

              if(mod(ts,21600) .eq. 0) then

               do ss=1,sp

                C_P_new(ss)=p0    !Concentration of Cnutrient in mili molar
                C_F_new(ss)=f0    !Concentration of nutrient Cin mili molar
                C_C_new(ss)=c0    !Concentration of nutrient Cin mili molar
                C_fi_new(ss)=fi0

               end do
            end if
       !do ss=,sp
       ! do i=1,tnob
        ! b_new(i,ss)=b(i,ss)
        !end do
       !end do

  do ss=1,sp
 
     do i=1,tnob
  
          pb(i,1) = delta*(c_p_new(ss)/(k_s+c_p_new(ss)))*(1.0/(exp(-ds(i,1)+1.0)))
          cb(i,2) = delta*(c_c_new(ss)/(k_s+c_c_new(ss)))*(1.0/(exp(-ds(i,2)+1.0)))
          fb(i,3) = delta*(c_f_new(ss)/(k_s+c_f_new(ss)))*(1.0/(exp(-ds(i,3)+1.0)))
          fib(i,4) =delta*(c_fi_new(ss)/(k_s+c_fi_new(ss)))*(1.0/(exp(-ds(i,4)+1.0)))
          
          pb1(i,1) =delta*(c_p_new(ss)/(k_s+c_p_new(ss)))
          cb1(i,2) =delta*(c_c_new(ss)/(k_s+c_c_new(ss)))
          fb1(i,3) =delta*(c_f_new(ss)/(k_s+c_f_new(ss)))
          fib1(i,4)=delta*(c_fi_new(ss)/(k_s+c_fi_new(ss)))



          alpha_p(i)=pb(i,1)/delta1
          alpha_c(i)=cb(i,2)/delta1
          alpha_f(i)=fb(i,3)/delta1
          alpha_fi(i)=fib(i,4)/delta1


          alpha_p1(i)=pb1(i,1)/delta1
          alpha_c1(i)=cb1(i,2)/delta1
          alpha_f1(i)=fb1(i,3)/delta1
          alpha_fi1(i)=fib1(i,4)/delta1




          ph(i,1) =delta2*(c_p_new(ss)/(k_s+c_p_new(ss)))*(1.0/(exp(-ds(i,1)+1.0)))
          ch(i,2) =delta2*(c_c_new(ss)/(k_s+c_c_new(ss)))*(1.0/(exp(-ds(i,2)+1.0)))
          fh(i,3) =delta2*(c_f_new(ss)/(k_s+c_f_new(ss)))*(1.0/(exp(-ds(i,3)+1.0)))
          fih(i,4)=delta2*(c_fi_new(ss)/(k_s+c_fi_new(ss)))*(1.0/(exp(-ds(i,4)+1.0)))
!
          pt(i,1)  =   pb(i,1)+ph(i,1)
          ct(i,2)  =   cb(i,2)+ch(i,2)
          ft(i,3)  =   fb(i,3)+fh(i,3)
          fit(i,4) =   fib(i,4)+fih(i,4)

                       ! do i=1,tnob   
                              asso3=0
                              asso4=0

                              do j=1,tnob
                                 if (bs_new(i,j) < 0.0) then
                                    asso3=asso3+bs_new(i,j)*b_new1(j,ss)
                                 end if
                                 if (bs_new(i,j) > 0.0 ) then
                                    asso4=asso4+bs_new(i,j)*b_new1(j,ss)
                                 end if
                              end do
                        !  end do
          growth_pn(i,1)=alpha_p(i)*dt*b_new1(i,ss)+alpha_p1(i)*dt*b_new1(i,ss)*(-sc*b_new1(i,ss)+asso3+asso4)/k
          growth_cn(i,2)=alpha_c(i)*dt*b_new1(i,ss)+alpha_c1(i)*dt*b_new1(i,ss)*(-sc*b_new1(i,ss)+asso3+asso4)/k
          growth_fn(i,3)=alpha_f(i)*dt*b_new1(i,ss)+alpha_f1(i)*dt*b_new1(i,ss)*(-sc*b_new1(i,ss)+asso3+asso4)/k
          growth_fin(i,4)=alpha_fi(i)*dt*b_new1(i,ss)+alpha_fi1(i)*dt*b_new1(i,ss)*(-sc*b_new1(i,ss)+asso3+asso4)/k


          deathbn(i) = death*dt*(b_new1(i,ss))
          growthn(i) = growth_pn(i,1)+growth_cn(i,2)+growth_fn(i,3)+growth_fin(i,4)

         growthfn(i)=growthn(i)
         b_newn(i,SS)=b_new1(i,ss)+growthfn(i)-deathbn(i)
         !write(6,*) b_newn(i,ss)     
       end do  

!!!!!!!!!!!!!!!!!!!***************************!!!!!!!!!!!!!!!!!!!

     d=mod(ts,ts1)
         if (d .eq. 0) then


                                  rr1=0.0
                                    do i=1,tnob

                                      rr1=rr1+((b_newn(i,ss)-b2(i,ss)))**2

                                    end do
  
                                      rr1=sqrt(rr1)/float(tnob)
                                   
                                        
                                    if(rr1 .lt. abs(1000000)) then        
                     
      !    write(29,*)(b_newn(i,ss), i=1,tnob)
                                                        
                                       exit ts2loop 

                                    else





                                       do i=1,tnob
                                          b2(i,ss)=b_newn(i,ss)
                                         end do

                                    end if  
        end if
                 

                            p_con=0.0
                            c_con=0.0
                            f_con=0.0
                            fi_con=0.0
                
                       do i=1,tnob
                            p_con=p_con+(b_new1(i,ss))*pt(i,1)
                            c_con=c_con+(b_new1(i,ss))*ct(i,2)
                            f_con=f_con+(b_new1(i,ss))*ft(i,3)
                            fi_con=fi_con+(b_new1(i,ss))*fit(i,4)
                       end do
                
                            C_P_newn(ss)=C_P_new(ss)-p_con
                            C_c_newn(ss)=C_c_new(ss)-c_con
                            C_f_newn(ss)=c_f_new(ss)-f_con
                            C_fi_newn(ss)=c_fi_new(ss)-fi_con
                
                  end do
                
                
                    do ss=1,sp   
                        C_P_new(ss)=c_P_newn(ss)
                        C_C_new(ss)=c_C_newn(ss)
                        C_F_new(ss)=c_F_newn(ss)
                        C_Fi_new(ss)=c_Fi_newn(ss)
                     end do

          do ss=1,sp
               do i=1,tnob

                                         b_new1(i,ss)=b_newn(i,ss)
                                    end do
              end do







 !        a =mod(ts,ts1)
 !        if (a .eq. 0) then
     
 !            do ss=1,sp

 !                   write(180,*) ts,(b_new1(i,ss),i=1,tnob)
        
 !            end do
     
 !        end if
    
   end do ts2loop


!!!!!!!!!!!!!!!!!!!!!!!!comparision of two RMSDs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    !rmsd1=0
    rmsd2=0
   ! rr3=0
  do ss=1,sp    
    do i=1,tnob  
     rmsd2=rmsd2+((b_new1(i,ss)-b_av(i,u))/b_av(i,u))**2
    end do      
  end do

    rmsd2=sqrt(rmsd2/float(tnob))

    cim=cim+1

! write(6,*) m, rmsd1,rmsd2,cim,acim
 
                 
          If (rmsd2 < rmsd1) then            
                  acim=acim+1
                 write(203,*) m, rmsd1,rmsd2,cim,acim,awm
                 write(6,*) m, rmsd1,rmsd2,cim,acim,awm
                         do i=1,tnob
                                do j =1,tnob
                                        bs(i,j)=bs_new(i,j)
                                end do
                         end do
                 do ss=1,sp
                            do i=1, tnob
                                   b(i,ss)=b_new1(i,ss)
                            end do
                end do
               
                do i= 1, tnob
                            write(un,*) (bs(i,j), j=1, tnob)
                end do
 
                rmsd1=rmsd2

                 do  ss=1,sp

                           write(180,*) ts,(b(i,ss),i=1,tnob)

                end do
          else if (rand() .lt. wm) then
               awm=awm+1
               write(6,*) m, rmsd1, rmsd2, cim, acim,awm
               write(203,*) m, rmsd1,rmsd2,cim,acim,awm
                  do i=1,tnob
                        do j =1,tnob
                           bs(i,j)=bs_new(i,j)
                        end do
                  end do

               do ss=1,sp

                    do i=1, tnob

                        b(i,ss)=b_new1(i,ss)

                    end do

                end do

               do i= 1, tnob
                    write(un,*) (bs(i,j), j=1, tnob)
               end do

              rmsd1=rmsd2
              !rmsd3=rmsd4

               do  ss=1,sp

                           write(180,*) ts,(b(i,ss),i=1,tnob)

                end do


          end if 
                    
             
              !!!!!!!!!!!!!!!!!!!$$$$$$$$$$$$$$$$$$$!!!!!!!!!!!!!!!!!!!!!!!!!!!


!write(45,*) m,b(1,2),b(1,4),b(2,1),b(2,3),b(3,2),b(3,4),b(4,1),b(4,3)
!write(45,*) m,bs(1,2),bs(1,4),bs(2,1),bs(2,3),bs(3,2),bs(3,4),bs(4,1),bs(4,3)

!write(um,*)m,bs(1,1),bs(1,2),bs(1,3),bs(1,4),bs(2,2),bs(2,3),bs(2,4),bs(3,3),bs(3,4),bs(4,4),&
!&bs(2,1),bs(3,1),bs(3,2),bs(4,1),bs(4,2),bs(4,3)

call cpu_time(stopTime)

tot=stopTime - startTime

write(555,*) m,tot



end do  ! number of steps

close(unit=un)
!close(unit=um)

end do

!close(203)
!close(445)
!close(37)


end program main


