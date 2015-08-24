
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <perfmon/pfmlib.h>

#include <perfmon/perfmon.h>
#include <perfmon/pfmlib_itanium2.h>


#define MAX_EVT_NAME_LEN        256
#define NUM_PMCS PFMLIB_MAX_PMCS
#define NUM_PMDS PFMLIB_MAX_PMDS

pfmlib_input_param_t inp;
pfarg_reg_t pd[NUM_PMDS];
	
int pfm_easy_init(int nCounters,char **sCounters) {
	int i, ret, fd;
	pfmlib_ita2_input_param_t ita2_inp;
	pfmlib_output_param_t outp;
	pfarg_reg_t pc[NUM_PMCS];

	pfarg_context_t ctx[1];
	pfarg_load_t load_args;
	char name[MAX_EVT_NAME_LEN];
	
	if (pfm_initialize() != PFMLIB_SUCCESS) {
		fprintf(stderr, "cannot initialize libpfm\n");
		return 0;
	}
  
	memset(pd, 0, sizeof(pd));
	memset(pc, 0, sizeof(pc));
	memset(ctx, 0, sizeof(ctx));
	memset(&inp,0, sizeof(inp));
	memset(&outp,0, sizeof(outp));
	memset(&load_args,0, sizeof(load_args));
	memset(&ita2_inp,0, sizeof(ita2_inp));
	
	/*
		* find event descriptor for our event
		*/
	
	if (nCounters>NUM_PMCS) {
		fprintf(stderr, "cannot use more than %d counters\n", NUM_PMCS);
		return 0;
	}

	
	for (i=0; i<nCounters; i++) {
		ret = pfm_find_event(sCounters[i],&inp.pfp_events[i].event);
		if (ret != PFMLIB_SUCCESS) {
			fprintf(stderr,"event %s not found\n", sCounters[i]);
			return 0;
		}
	}
	/*
	* indicate that we are using the PMC8 opcode matcher
	*/
	ita2_inp.pfp_ita2_pmc8.opcm_used = 1;
	
	/*
		* load value to install in PMC8 (some control fields may be 
		* modified by library). Here it is the pattern for the
		* br.cloop instruction.
		*/
	
	ita2_inp.pfp_ita2_pmc8.pmc_val = 0x1400028003fff1fa;
	
	/*
	* set the default privilege mode for all counters:
	*   PFM_PLM3 : user level only
	*/
	inp.pfp_dfl_plm = PFM_PLM3; 
	
	/*
	* how many events we are interested in
	*/
	inp.pfp_event_count = nCounters;
	
	/*
		* let the library figure out the values for the PMCS
		*/
	ret = pfm_dispatch_events(&inp, &ita2_inp, &outp, NULL);
	if (ret != PFMLIB_SUCCESS) {
		fprintf(stderr, "cannot configure events: %s\n", pfm_strerror(ret));
		return 0;
	}
	/*
		* copy the library parameters to the OS-specific structures.
		* Here we propagate the PMC indexes and values.
		*/
	for (i=0; i < outp.pfp_pmc_count; i++) {
		pc[i].reg_num   = outp.pfp_pmcs[i].reg_num;
		pc[i].reg_value = outp.pfp_pmcs[i].reg_value;
	}
	/*
		* propagate the PMC indexes to the PMD arguments to the
		* kernel. This is required for counting monitors.
		*/
	for (i=0; i < inp.pfp_event_count; i++) {
		pd[i].reg_num   = pc[i].reg_num;
	}
	/*
		* now create the context
		*/
	ret = perfmonctl(0, PFM_CREATE_CONTEXT, ctx, 1);
	if (ret == -1) {
		fprintf(stderr, "PFM_CREATE_CONTEXT errno %d\n", errno);
		return(0);
	}
	/*
		* extract the file descriptor identifying the context
		*/
	fd = ctx[0].ctx_fd;
	
	/*
		* Now program the PMC registers.
		* In this case, we write two PMC registers
		*/
	ret = perfmonctl(fd, PFM_WRITE_PMCS, pc, outp.pfp_pmc_count);
	if (ret == -1) {
		fprintf(stderr, "PFM_WRITE_PMCS errno %d\n",errno);
		return(0);
	}
	/*
		* We reset the PMDs that go with the PMCs
		*/
	ret = perfmonctl(fd, PFM_WRITE_PMDS, pd, inp.pfp_event_count);
	if (ret == -1) {
		fprintf(stderr, "PFM_WRITE_PMDS errno %d\n",errno);
		return 0;
	}
	/*
		* attach the perfmon context to ourself
		*/
	load_args.load_pid = getpid();
	ret = perfmonctl(fd, PFM_LOAD_CONTEXT, &load_args, 1);
	if (ret  == -1) {
		fprintf(stderr, "PFM_LOAD_CONTEXT errno %d\n",errno);
		return(0);
	}
	return(fd);
}

void pfm_easy_read(int fd, long val[]) {
	int ret, i;
	ret = perfmonctl(fd, PFM_READ_PMDS, pd, inp.pfp_event_count);
	if (ret == -1) {
		fprintf(stderr, "PFM_READ_PMDS errno %d\n",errno);
		exit(1);
	}

	for (i=0; i < inp.pfp_event_count; i++) {
		val[i] = pd[i].reg_value;
	}
	close(fd);
}