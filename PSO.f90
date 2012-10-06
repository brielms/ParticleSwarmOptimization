module globals
	implicit none 
	integer, parameter :: wp = selected_real_kind(p=14)
	
	contains
	
	pure function test(x)
		implicit none
		real(wp) :: test
		real(wp), dimension(2), intent(in) :: x
		
		test = 2._wp*x(1)**2-1._wp+2._wp*x(2)**2-1._wp
		
	end function test

end module globals

program newPSO
	use globals
	implicit none
	real(wp), dimension(2) :: inpt

	inpt(1) = 1._wp
	inpt(2) = 1._wp

	call PSO(test,inpt,100)

	contains
	
	
	
	!!
	! 
	!    fn   : pass the objective function to be optimized. 
	!  params : the arguments required by fn
	! MaxItter: Maximum number of itterations for PSO to perform.
	! 
	!!
	subroutine PSO(fn,params,MaxItter)
		use m_mrgref
		use globals
		implicit none
		integer :: i,j,dim,bugs,itter,MaxItter
		integer, dimension(:), allocatable :: R
		real(wp), external :: fn
		real(wp) :: phig,phir,phiv,fbestposever
		real(wp), dimension(:) :: params
		real(wp), dimension(:),  allocatable :: fbuglist,bestposever,rnd2,bestfbuglist,rnd3,rnd4
		real(wp), dimension(:,:),allocatable :: buglist,vlist,rnd1,bestoldpos

		! Optimization parameters
		phig = 2.1_wp
		phir = 1.9_wp
		phiv = 0.2_wp
		! Number of optimizers (bugs in the swarm)
		bugs = 20

		! dim : dimension of the parameter vector
		dim = size(params,1)

		! Distribute the bugs around the initial guess vector, params
		allocate(buglist(bugs,dim),rnd1(bugs,dim))
		call random_number(rnd1)
		forall(i=1:bugs,j=1:dim)
			buglist(i,j) = params(j)+params(j)*(rnd1(i,j)-0.5_wp)
		end forall

		! Determine where the bugs are now
		allocate(fbuglist(bugs))
		!!!!!!!!!! I'd love to replace this with a FORALL loop, but
		!!!!!!!!!!  guess you can't have a pure & external function.
		do i = 1, bugs
				fbuglist(i) = fn(buglist(i,:))
		end do

		! D_MRGREF ranks the vector fbuglist in ascending order onto R
		! 		i.e. fbugist(R(1)) will hold the smallest value,
		!		fbuglist(R(bugs)) will hold the largest value. 
		allocate(R(bugs))
		call D_MRGREF(fbuglist,R)


		! Giving the bugs some initial velocity.
		!! Academics seem divided on this topic, use either this, or a random initial velocity.
		allocate(rnd2(bugs),vlist(bugs,dim))
		forall(i=1:bugs)
			vlist(i,:) = rnd2(i)*phig*( buglist(R(1),:)-buglist(i,:) )
		end forall


		! Holds each bug's best previously help position.
		allocate(bestoldpos(bugs,dim))
		bestoldpos = buglist
		! Holds each the fn value at each bug's best previously held position.
		allocate(bestfbuglist(bugs))
		bestfbuglist = fbuglist
		! These are the best fn value and the parameters to get to that fn value that the swarm has ever seen.
		fbestposever = fbuglist(R(1))
		bestposever =  bestoldpos(R(1),:)

		allocate(rnd3(bugs),rnd4(bugs))

		! Main optimization loop
		do itter = 1, MaxItter
			! Update the bugs position.
			buglist = buglist + vlist
			
			! Determine where the bugs are now.
			do i = 1, bugs
					fbuglist(i) = fn(buglist(i,:))
			end do
 	
	    	! Check if any new postion is better than any previous `best'.
		 	do i=1,bugs
				if(fbuglist(i).le.bestfbuglist(i)) then
					bestfbuglist(i) = fbuglist(i)
	 				bestoldpos(i,:) = buglist(i,:)
				end if
			end do
			
			! Reranking fbuglist.
			call D_MRGREF(fbuglist,R)
			
			! Check to see if the best new postion beats out the previously known best position. 
			if( fbuglist(R(1)).lt.fbestposever )then
	 	   	bestposever = buglist(R(1),:)
	 	   	fbestposever = fbuglist(R(1))
			end if
			
			! New velocity update formula
			call random_number(rnd3)
			call random_number(rnd4)
			forall(i=1:bugs)
				vlist(i,:) = phiv*vlist(i,:)+rnd3(i)*phig*(bestposever(:)-buglist(i,:) )+rnd4(i)*phir*( bestoldpos(i,:)-buglist(i,:) )
			end forall
			
			write(*,*) fbestposever
			
		end do


	end subroutine PSO

end program newPSO








































