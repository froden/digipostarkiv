//
//  main.m
//  DPSync
//
//  Created by Frode Nerbråten on 22.04.14.
//  Copyright (c) 2014 Nerbraten. All rights reserved.
//

//#import <Cocoa/Cocoa.h>
#import "HsCocoa_stub.h"
#import "Rts.h"

int main(int argc, char * argv[])
{
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsAll;
    conf.rts_opts = "-V0";
    hs_init_ghc(&argc, &argv, conf);
    
    return NSApplicationMain(argc, (const char **) argv);
}
