test_that("gausswindow fails on incorrect inputs",{
   expect_error(gausswindow(l=-1))
   expect_error(gausswindow(l= 0))
   expect_error(gausswindow(l= 1))

   expect_error(gausswindow(w=-1))
   expect_error(gausswindow(w= 0))
})

test_that("mt_smooth fails (only) on incorrect inputs", {

   mt_fixture <- mt_time_normalize(mt_example,save_as="trajectories")

   # Default parameters should just work
   expect_no_error(mt_smooth(mt_fixture))


   # Window size should be checked independently of windowing function
   wdw <- function(l,..) rep(1,max(l,1))

   expect_error(mt_smooth(mt_fixture,l=-1,window=wdw))
   expect_error(mt_smooth(mt_fixture,l= 0,window=wdw))
   expect_error(mt_smooth(mt_fixture,l= 1,window=wdw))

   expect_no_error(mt_smooth(mt_fixture,l= 2,window=wdw))
   expect_no_error(mt_smooth(mt_fixture,l= 10,window=wdw))

   # Check that incorrect combinations of causal and symmetric are reported
   expect_error(mt_smooth(mt_fixture,symmetric=T,causal=T))
   expect_no_error(mt_smooth(mt_fixture,symmetric=F,causal=T,l=11))
   expect_no_error(mt_smooth(mt_fixture,symmetric=T,causal=F))

   # Check that causal filtering requires an odd length
   expect_error(mt_smooth(mt_fixture,causal=T,l=10))
   expect_no_error(mt_smooth(mt_fixture,causal=T,l=11))

})
