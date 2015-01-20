//
//  Sync.m
//  Digipostarkiv
//
//  Created by Frode on 20/01/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

#import "Sync.h"
#import <Foundation/Foundation.h>
#import "HsCocoa_stub.h"

@implementation Sync

+(NSString*)oauthUrl: (NSString*)state {
    char *cAuthUrl = hsAuthUrl("state");
    return [NSString stringWithFormat:@"%s" , cAuthUrl];
}

+(void)logout {
    hsLogout();
}

@end