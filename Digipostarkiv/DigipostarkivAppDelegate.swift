//
//  DigipostarkivAppDelegate.swift
//  Digipostarkiv
//
//  Created by Frode on 02/02/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Cocoa

class DigipostarkivAppDelegate: NSObject, NSApplicationDelegate {
    
    @IBOutlet weak var loginWindowController: LoginWindowController!
    @IBOutlet weak var menuController: MenuController!
    
    var syncInProgress: Bool
    var syncTimer: Timer?
    var runNumber = 0
    
    override init() {
        syncInProgress = false
        syncTimer = nil
    }
    
    func applicationWillFinishLaunching(_ notification: Notification) {
        NSAppleEventManager.shared().setEventHandler(
            self,
            andSelector: #selector(handleURLEvent(_:replyEvent:)),
            forEventClass: AEEventClass(kInternetEventClass),
            andEventID: AEEventID(kAEGetURL))
    }
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        Sync.initSync()
        startSyncTimer()
    }
    
    func isStarted() -> Bool {
        return syncTimer != nil && syncTimer!.isValid
    }
    
    func startSyncTimer() {
        if (!isStarted()) {
            NSLog("starting sync timer")
            syncTimer = Timer.scheduledTimer(timeInterval: 10.0, target: self, selector: #selector(sync), userInfo: nil, repeats: true)
        }
        syncTimer!.fire()
    }
    
    func stopSyncTimer() {
        if (isStarted()) {
            NSLog("stopping sync timer")
            syncTimer!.invalidate()
            syncTimer = nil
        }
    }
    
    func sync() {
        if (Sync.isLoggedIn()) {
            DispatchQueue.global(priority: DispatchQueue.GlobalQueuePriority.default).async(execute: {
                self.detectChangeAndSync()
            })
        } else {
            stopSyncTimer()
            loginWindowController.showOauthLoginPage()
        }
    }
    
    func detectChangeAndSync() {
        if (syncInProgress) {
            return;
        }
        syncInProgress = true;
        let checkRemote = runNumber % 6 == 0
        runNumber = runNumber + 1
        var remoteSync = false
        if (checkRemote) {
            let remoteChangeResult = Sync.hasRemoteChange()
            if (remoteChangeResult == 0) {
                remoteSync = true
                NSLog("Remote change detected")
            } else if (remoteChangeResult == 1) {
                DispatchQueue.main.async(execute: {self.loginWindowController.showOauthLoginPage()})
                return
            } else if (remoteChangeResult == 99){
                NSLog("Unhandled syncresult from hsRemoteChange")
            }
        }
        let localSync = !checkRemote && Sync.hasLocalChange()
        if localSync {
            NSLog("Local change detected")
        }
        if (localSync || remoteSync) {
            fullSync()
        }
        syncInProgress = false;
    }
    
    func fullSync() {
        objc_sync_enter(self)
        defer { objc_sync_exit(self) }
        
        menuController.showActiveIcon()
        let result = Sync.sync();
        if (result != 0) {
            if (result == 1) {
                DispatchQueue.main.async(execute: {self.loginWindowController.showOauthLoginPage()})
            } else {
                //TODO: give up after n failures?
                NSLog("Unhandled syncresult: %i", result);
            }
        }
        menuController.showStandardIcon()
    }
    
    func handleURLEvent(_ event: NSAppleEventDescriptor, replyEvent: NSAppleEventDescriptor) {
        
        let urlString = event.paramDescriptor(forKeyword: AEKeyword(keyDirectObject))?.stringValue
        let url = urlString.flatMap { URL(string: $0) }
        let authCode = url.flatMap(parseCode)
        
        if let code = authCode {
            let result = Sync.getAccessToken("state", code as String)
            if (result == 0) {
                startSyncTimer()
                loginWindowController.closeOauthLoginPage()
            } else {
                NSLog("Error from getAccessToken: %i", result)
            }
        } else {
            NSLog("No auth code found in url")
        }
        
    }
    
    func parseCode(_ url: URL) -> NSString? {
        let urlComponents = url.query?.components(separatedBy: "&")
        let codeCompoents = urlComponents?
            .map{$0.components(separatedBy: "=")}
        let codes = codeCompoents?
            .filter{$0.count == 2 && $0[0] == "code"}
            .first
        return codes?[1] as NSString?
    }
    
}
