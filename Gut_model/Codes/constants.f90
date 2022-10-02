        module constants
          implicit none

         
        real, parameter :: N=1E6 ! total number of bacteria in largeintestine
        character(22) :: filename,abc,ab,filename1
        !real,parameter  ::l=250.0  !Total length of large intestine (in
        !cm) 
        real,parameter  ::l=300.0
        real,parameter::c=0.008 !drift of protein in cm^2 per hour  
        integer, parameter :: t=6*3600 !Total time in seconds
        real, parameter ::dt=1.0
        !real, parameter :: dx=0.04
        real, parameter :: dx=1.0
        integer,parameter :: sp=int(l/dx)
        integer, parameter ::tp=int(t/dt)
        real, parameter :: bt=(N)/float(sp)
                !!!!!!!Giving controlled diet 86g of protein, 81g of fat
                !and
                !373g of carbohydrates !!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!! equal calories diet!!!!!!!!!!!!!!!!!!
       real,parameter :: p0=1.25  ! in hecto gram
        real,parameter :: c0=1.25 ! in hectogram
        real,parameter :: f0=0.56 ! in hecto gram 
                !real,parameter :: c0=2.8 ! in hectogram
        real,parameter :: fi0=17.5
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        real:: D_b=0.0 !Difusion coefficient of bacteri
        integer,parameter :: ts1=int(3600.0/1.0)
        real,parameter :: delta2= 0.2E-12*0.25 !Nutrient consumption for host
        real,parameter :: delta= 1E-15*0.25  !Nutrient consumption perbacteria per second 
        real,parameter :: delta1=1.0E-12 ! mass of a bacteria
        real,parameter :: death=0.0006*0.005*0.03
        real,parameter :: K_s =5.0 ! concentration of nutrient when thegrowth rate is half of its maximum value in mii molar.
        real ::cfl
        integer,parameter :: tnob=14,tnon=4
        real,parameter :: cap = 1.0E12, k1=1.0E07, k3=1.0E06,k9=5.0E06
        real,parameter :: ae=1000
        integer, parameter :: ub=14, lb=1,ub1=3,lb1=1
        real, parameter :: sc=7.0
        real, parameter  :: r1=0.05
!        real, parameter :: fac=$f
 
       end module constants 
