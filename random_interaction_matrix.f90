        program main
        implicit none
         integer , parameter :: tnob = 14   
         real, dimension (tnob,tnob) :: bs
         integer :: i,j,seed,SEED1
         real*8 :: r1, r2, r3,ran3
         real, parameter :: p1=
       
         real,parameter :: sc=p1*14/2.0
       
       open(11, file= 'bs4.dat' , status = 'new', action = 'write')
       !call random_seed() 
       seed=23456432
       !seed1=seed+1 
              do i=1,tnob

          do j =1,tnob
          !seed=23456432
             !call random_number(r1)
             !r1=rand()
             r2=ran3(seed)
             
             !r3=ran3(seed1) 
       !      print*, r2,r3
             if (j .eq. i ) then

                 bs(i,j)=(2*p1*r2-p1)-sc

             else if (i .ne. j) then

                 bs(i,j) = (2*p1*r2-p1)

            ! else if (j .lt. i) then

            !     bs(i,j)=bs(j,i)
             end if
           end do

       end do





 ! write(6,*) 'hi'
 
      do i = 1, tnob
    
           write(11,*) (bs(i,j), j=1, tnob)

      end do

  close(11)


      do i = 1, tnob
          
           write(6,*) (bs(i,j), j=1, tnob)

      end do


   end 
   FUNCTION ran3(idum)
      INTEGER*4 idum
      INTEGER*4 MBIG,MSEED,MZ
!      REAL MBIG,MSEED,MZ
      REAL*8 ran3,FAC
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1./MBIG)
!     PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=1./MBIG)
      INTEGER*4 i,iff,ii,inext,inextp,k
      INTEGER*4 mj,mk,ma(55)
!     REAL mj,mk,ma(55)
      SAVE iff,inext,inextp,ma
      DATA iff /0/
      if(idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=MSEED-iabs(idum)
        mj=mod(mj,MBIG)
        ma(55)=mj
        mk=1
        do 11 i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.MZ)mk=mk+MBIG
          mj=ma(ii)
11      continue
        do 13 k=1,4
          do 12 i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.MZ)ma(i)=ma(i)+MBIG
12        continue
13      continue
        inext=0
        inextp=31
        idum=1
      endif
      inext=inext+1
      if(inext.eq.56)inext=1
      inextp=inextp+1
      if(inextp.eq.56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.MZ)mj=mj+MBIG
      ma(inext)=mj
      ran3=mj*FAC
      return
      END
