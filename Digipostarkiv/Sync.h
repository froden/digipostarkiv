//
//  Sync.h
//  Digipostarkiv
//
//  Created by Frode on 20/01/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

#import <Foundation/Foundation.h>

#ifndef Digipostarkiv_Sync_h
#define Digipostarkiv_Sync_h

@interface Sync : NSObject 

+(NSString*)oauthUrl: (NSString*)state;
+(NSString*)initSync;
+(void)logout;
+(int)getAccessToken: (NSString*)state :(NSString*)authCode;
+(BOOL) isLoggedIn;
+(int)sync;
+(int)hasRemoteChange;
+(BOOL)hasLocalChange;

@end

#endif
