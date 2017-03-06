//
//  MainMenuController.swift
//  Digipostarkiv
//
//  Created by Frode on 19/01/15.
//  Copyright (c) 2015 Nerbraten. All rights reserved.
//

import Foundation
import Cocoa

class MenuController: NSObject {
    
    @IBOutlet weak var statusMenu: NSMenu!
    var statusItem: NSStatusItem!
    var statusImage: NSImage!
    var statusImageActive: NSImage!
    
    
    @IBOutlet weak var loginWindowController: LoginWindowController!
    @IBOutlet weak var appDelegate: DigipostarkivAppDelegate!
    
    override func awakeFromNib() {
        statusItem = NSStatusBar.system().statusItem(withLength: NSVariableStatusItemLength)
        statusItem!.menu = statusMenu
        let statusImageSize = NSSize(width: 14.44, height: 19.0)
        statusImage = NSImage(named: "digipost-black.png")
        statusImage?.size = statusImageSize
        statusImageActive = NSImage(named: "digipost-red.png")
        statusImageActive?.size = statusImageSize
        let altStatusImage = NSImage(named: "digipost-white.png")
        altStatusImage?.size = statusImageSize
        statusItem!.image = statusImage
        statusItem!.alternateImage = altStatusImage
        statusItem!.highlightMode = true
    }
    
    @IBAction func exitApp(_ sender: NSMenu) {
        hs_exit()
        NSApplication.shared().terminate(self)
    }
    
    @IBAction func openInFinder(_ sender: NSMenu) {
        let homePath = NSHomeDirectory() + "/Digipostarkiv"
        NSWorkspace.shared().openFile(homePath)
    }
    
    @IBAction func manualSync(_ sender: NSMenu) {
        if (Sync.isLoggedIn()) {
            DispatchQueue.global(priority: DispatchQueue.GlobalQueuePriority.default).async(execute: {
                self.appDelegate.fullSync()
            })
        } else {
            appDelegate.stopSyncTimer()
            loginWindowController.showOauthLoginPage()
        }
    }
    @IBAction func openDigipostInBrowser(_ sender: AnyObject) {
        let digipostUrl = URL(string: "https://www.digipost.no/app/#/")
        NSWorkspace.shared().open(digipostUrl!)
    }
    
    @IBAction func startSync(_ sender: NSMenu) {
        appDelegate.startSyncTimer()
    }
    
    @IBAction func stopSync(_ sender: AnyObject) {
        appDelegate.stopSyncTimer()
    }
    
    override func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
        let started = appDelegate.isStarted()
        if (menuItem.action == #selector(startSync)) {
            menuItem.isHidden = started
            return !started
        } else if (menuItem.action == #selector(stopSync)) {
            menuItem.isHidden = !started
            return started
        } else {
            return true
        }
    }
    
    func showActiveIcon() {
        statusItem?.image = statusImageActive
    }
    
    func showStandardIcon() {
        statusItem?.image = statusImage
    }
}
