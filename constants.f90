      module  constants
        implicit none

      real, parameter :: N=1E10 ! total number of bacteria in largeintestine

        character(22) :: filename,abc,ab,filename1

        !real,parameter  ::l=250.0  !Total length of large intestine (in
        !cm) 
        real,parameter  ::l=1.0
        real,parameter::c=0.008 !drift of protein in cm^2 per hour  
        !integer, parameter :: t=1000*24*3600 !Total time in seconds
        integer, parameter :: t=5184000
        real, parameter ::dt=1.0
        !real, parameter :: dx=0.04
        real, parameter :: dx=1.0
        integer,parameter :: sp=int(l/dx)
        integer, parameter ::tp=int(t/dt)
        real, parameter :: bt=(N)/float(sp)
                !!!!!!!Giving controlled diet 86g of protein, 81g of fat
                !and
                !373g of carbohydrates !!!!!!!!!!!!
        real,parameter :: p0=12.5! in hecto gram
        real,parameter :: f0=5.6 ! in hecto gram 
        real,parameter :: c0=12.5 ! in hectogram
        real,parameter :: fi0=25.0

        real:: D_b=0.0 !Difusion coefficient of bacteri

        integer,parameter :: ts1=int(21600/1.0)

                real,parameter :: delta2= 0.2E-12*0.25*0.0 !Nutrientconsumption for host
                !real,parameter :: delta= 2.0E-16*0.25  !Nutrientconsumption per bacteria per second 
                real,parameter :: delta= 2.0E-16  !Nutrientconsumption per bacteria per second 
                real,parameter :: delta1=1.0E-12 ! mass of a bacteria
        real,parameter :: death=0.0006*0.009
        real,parameter :: K_s = 12.5/4.0 ! concentration of nutrient when the growth rate is half of its maximum value in mili molar.
        real ::cfl
        integer,parameter :: tnob=14,tnon=4
        real,parameter :: k = 1.0E12, k1=1.0E07, k3=1.0E06,k9=5.0E06
        real,parameter :: ae=1000
        integer, parameter :: ub=14, lb=1,ub1=3,lb1=1
        real, parameter :: sc=0.0
        !real, parameter :: sc=1.0
 
        real, parameter  :: r1=0.05
        real,parameter :: st = 0.1 
        real,parameter :: ctm = 0.05
        integer,parameter :: tnos = 2500000
        real, parameter :: wm=0.15
        integer, parameter :: tons=1
      end module constants

