        program main
         use constants
        implicit none
        !        character(22) :: filename,abc,abc1,ab,filename1
        real, dimension(tnob,tnon)::ds,pb,cb,fb,fib,ph,ch,fh,fih,pt,ct,ft,fit,pb1
        real,dimension(tnob,tnon) :: growth_p,growth_c,growth_f,growth_fi
        real,dimension(tnob,tnon) :: growth_pn,growth_cn,growth_fn,growth_fin
        real, dimension(tnob,tnob) :: bs,ec,bs_new,bs1,bs_ex
        real,dimension(tnob,sp):: b,b_new,br, br_new,br_newn,b_newn,b_com1,b_com2
        integer :: i, j,ss,ts,xp1,xm1,un,m,um,q,mm,k
        real, dimension(tnon,sp) :: x, c_p, c_f, c_c,c_fi, c_p_new, c_f_new,c_c_new,c_fi_new,&
         & c_p_newn, c_f_newn,c_c_newn,c_fi_newn
        real, dimension(sp) :: rn, ln
        real ::asso,c_con,f_con,fi_con,a,asso1,asso2,asso3,asso4
        real,dimension(tnob,tnon) ::alpha_p,alpha_c,alpha_f,alpha_fi
        real,dimension(tnob) ::deathb,growth,growthf,k0,b_av,b_fi,p_con
        real,dimension(tnob,tnon) ::alpha_pn,alpha_cn,alpha_fn,alpha_fin,deathbn,growthn,growthfn,re,re1
        real :: bsum,t1,rmsd1,rmsd2
        real :: ri3,rmsd   
        integer :: ri1,ri2,ri4,ri5,f
        real ::rr0, rrr0, rr1 , rr2, rr3,rr4,rr5,rr6,re2,re3,btav,ree
        integer :: cim, acim,d,tss,d1
        real:: e


        cim=0
        btav=0
        acim=0
        un=7   ! Initial value is 7 is because 6 is used to write on screen 
        um=7
         cfl=c*(dt/dx)

        open(101, file='bd.dat',action='read',status='old')
        open(102, file='in_ma.dat',action='read',status='old')
        open(103, file='pop.dat',action='read',status='old')
        
        open(18, file='pop.txt',action='write',status='new')
        open(19, file='growth.txt',action='write',status='new')
        open(20, file='nutrient.txt',action='write',status='new')        
        
        open(181, file='pop_160.txt',action='write',status='new')        
        open(182, file='pop_210.txt',action='write',status='new')        
        open(182, file='pop_240.txt',action='write',status='new')        
 
        
       do i = 1, tnob
        read(101,*) (ds(i,j), j=1, tnon)

     end do
        close(101)
     do i = 1, tnob

       read(102,*) (bs(i,j), j=1, tnob)
     end do
        close(102)


do ss=1,sp
do i=1,tnob
!read(103,*)(b(i,ss), i=1,tnob)
b(i,ss)=N/float(tnob)
end do
end do
close(103)

!do ss=1,sp
!write(6,*)(b(i,ss), i=1,tnob)
!end do

!do i=1,tnob
!write(6,*)(ds(i,j), j=1,tnon)
!end do

!              do ss=1,sp

 !               C_P(1,ss)=p0    !Concentration of Cnutrient in mili molar
  !              C_p(2,ss)=f0    !Concentration of nutrient Cin mili molar
   !             C_p(3,ss)=c0    !Concentration of nutrient Cin mili molar
    !            C_p(4,ss)=fi0

     !          end do


do m=1,100
bsum=0.0
do ss=1,sp
  do i=1, tnob
    bsum=bsum+b(i,ss)
  end do
end do


write(6,*) m

           do ss=1,sp

     !            C_P(1,ss)=p0    !Concentration of Cnutrient in mili molar
     !            C_p(2,ss)=c0    !Concentration of nutrient Cin mili molar
     !            C_p(3,ss)=f0    !Concentration of nutrient Cin mili molar
     !            C_p(4,ss)=fi0

                  C_P(1,ss)=p0*exp(-((ss*dx-120.0)/30.0)**2)    
                  C_p(2,ss)=f0*exp(-((ss*dx-120.0)/30.0)**2)   
                  C_p(3,ss)=c0*exp(-((ss*dx-120.0)/30.0)**2)   
                  C_p(4,ss)=f0*exp(-((ss*dx-120.0)/30.0)**2)


            end do


do ss=1,sp

if(ss .eq. 1) then
rn(ss)=ss+1
ln(ss)=ss
else if (ss .eq. sp) then
rn(ss)=ss
ln(ss)=ss-1
else
rn(ss)=ss+1
ln(ss)=ss-1
end if

end do



ts1loop:  do ts=1,tp
          


    do ss=1,sp

    xp1=rn(ss)
    xm1=ln(ss)

       do i=1,tnob   ! number of bacteria

          do k=1,tnon  ! number of nutrient

              !write(6,*) ss, i, k, ds(i,k)
              pb1(i,k) = delta/delta1

          end do
         
          !if (m==1) then
           !   write(19,*)m,i,(k,pb1(i,k), k=1,tnon) 
          !end if      
!close(19)         
         do k=1, tnon
             !write(6,*)m,ts,c_P(k,ss)
            
              pb(i,k)=delta*(c_p(k,ss)/(k_s+c_p(k,ss)))*(1.0/(exp(-ds(i,k))+1.0))
             
              alpha_p(i,k)=pb(i,k)/(delta1)
             
              ph(i,k) =delta2*(c_p(k,ss)/(k_s+c_p(k,ss)))*(1.0/(exp(-ds(i,k)+1.0)))

              pt(i,k) = pb(i,k)+ph(i,k)
              
              !write(6,*) m,ts,i,k,pt(i,k)
          end do  !number of nutrient
          asso=0

         
      !    if (m==1) then
      !       write(19,*)m,i,(k,alpha_p(i,k), k=1,tnon) 
      !    end if      
          
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


         do k=1,tnon
              growth_p(i,k)=alpha_p(i,k)*dt*b(i,ss)+b(i,ss)*pb1(i,k)*(-sc*(b(i,ss))+asso1+asso2)/(cap)
         end do
          
           deathb(i) = death*dt*(b(i,ss))
           growth(i) = growth_p(i,1)+growth_p(i,2)+growth_p(i,3)+growth_p(i,4)
        
           growthf(i)=growth(i)
           b_new(i,SS)=b(i,ss)+growthf(i)-deathb(i)
      end do
                           
                        do  k=1, tnon
                            p_con(k)=0.0
                        end do
                  
                   do k=1,tnon 

                       do i=1,tnob
                           p_con(k)=p_con(k)+(b(i,ss))*pt(i,k)
                          ! write(6,*) m,ts,p_con(k)
                       end do  
                   
                   end do

                        do k=1,tnon
                            C_P_new(k,ss)=C_P(k,ss)*(1-cfl)+cfl*C_P(k,xm1)-p_con(k)
                       end do

       end do   !  ss loop
                
                
                    do ss=1,sp 
                      do k=1,tnon  
                        C_P(k,ss)=c_P_new(k,ss)
                   !     write(6,*) c_p(k,ss)
                      end do
                   end do
     
      do ss=1,sp
         do i=1,tnob
               b(i,ss)=b_new(i,ss)
         end do
      end do

    if (m .eq. 100) then

         d=mod(ts,ts1)
         
        
         if (d .eq. 0) then
 
               write(abc,'(I0)') ts
   
               filename="rho1_"//trim(abc)//".txt"
               
                un=un+1
                 open(unit=un,file=filename)   
                   do ss=1,sp

 
!                      write(18,*) (b(i,ss), i=1,tnob)

                      write(un,*) (c_p(k,ss), k=1,tnon)         


                end do
         close(unit=un)
         end if
    end if






!do ss=1,sp
!
!
      d1=mod(ts,ts1)
!
!
           if (d1 .eq. 0) then
                  if  (m==100 .and. ts==21600 ) then 
                  
                          do ss=1,sp

                               write(18,*)ss,(b(i,ss), i=1,tnob)

                          end do

                           
                           write(181,*)(b(i,160), i=1,tnob)
                           write(182,*)(b(i,210), i=1,tnob)
                           write(183,*)(b(i,240), i=1,tnob)

                  end if


                  if  (m==100 ) then

                          do ss=1,sp

                               write(20,*)ss,(c_p(k,ss), k=1,tnon)

                          end do
                  end if

        ! close(unit=um)
         end if
!end do
end do ts1loop


end do

close(18)
close(20)
end program main
 
