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

+(NSString*)initSync {
    char *syncDir = hsInitSync();
    return [NSString stringWithFormat:@"%s" , syncDir];
}

+(void)logout {
    hsLogout();
}

+(int)getAccessToken: (NSString*)state :(NSString*)authCode {
    return hsAccessToken((void*)state.UTF8String, (void*)authCode.UTF8String);
}

+(BOOL)isLoggedIn {
    return hsLoggedIn();
}

+(int)sync {
    return hsSync();
}

+(int)hasRemoteChange {
    return hsRemoteChanges();
}

+(BOOL)hasLocalChange {
    return hsLocalChanges();
}

@end