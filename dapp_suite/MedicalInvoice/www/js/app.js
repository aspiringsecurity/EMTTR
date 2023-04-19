// Ionic Starter App

// angular.module is a global place for creating, registering and retrieving Angular modules
// 'starter' is the name of this angular module example (also set in a <body> attribute in index.html)
// the 2nd parameter is an array of 'requires'
// 'starter.services' is found in services.js
// 'starter.controllers' is found in controllers.js
angular.module('starter', ['ionic', 'starter.controllers', 'starter.services'])

.run(function($ionicPlatform) {
  $ionicPlatform.ready(function() {
    // Hide the accessory bar by default (remove this to show the accessory bar above the keyboard
    // for form inputs)
    if (window.cordova && window.cordova.plugins && window.cordova.plugins.Keyboard) {
      cordova.plugins.Keyboard.hideKeyboardAccessoryBar(true);
    }
    if (window.StatusBar) {
      // org.apache.cordova.statusbar required
      StatusBar.styleLightContent();
    }
    loadProducts(); /******* Uncomment for InappPurchase *******/
    if(!window.localStorage['didTutorial']){
        window.localStorage['didTutorial'] = "false";
   }
  });
})

.config(function($stateProvider, $urlRouterProvider) {

  // Ionic uses AngularUI Router which uses the concept of states
  // Learn more here: https://github.com/angular-ui/ui-router
  // Set up the various states which the app can be in.
  // Each state's controller can be found in controllers.js
        $stateProvider
        
        // setup an abstract state for the tabs directive
        .state('tab', {
               url: "/tab",
               abstract: true,
               templateUrl: "templates/tabs.html"
               })
        
        // Each tab has its own nav history stack:
        
        .state('tab.home', {
               url: '/home',
               views: {
               'tab-home': {
               templateUrl: 'templates/tab-home.html',
               controller: 'HomeCtrl'
               }
               }
               })
        .state('tab.options', {
               url: '/options',
               views: {
               'tab-options': {
               templateUrl: 'templates/tab-options.html',
               controller: 'OptionsCtrl'
               }
               }
               })
        
        
        .state('intro', {
               url: '/intro',
               templateUrl: 'templates/introduction.html',
               controller: 'IntroCtrl'
               })
        
        
        .state('tab.list', {
               url: '/list',
               views: {
               'tab-list': {
               templateUrl: 'templates/tab-list.html',
               controller: 'ListCtrl'
               }
               }
               })
        
        .state('tab.purchase', {
               url: '/purchase',
               views: {
               'tab-purchase': {
               templateUrl: 'templates/tab-purchase.html',
               controller: 'PurchaseCtrl'
               }
               }
               })
        
        .state('tab.account', {
               url: '/account',
               views: {
               'tab-account': {
               templateUrl: 'templates/tab-account.html',
               controller: 'AccountCtrl'
               }
               }
               })
        
        .state('tab.listfilesWeb', {
               url: '/options/listfilesWeb',
               views: {
               'tab-options': {
               templateUrl: 'templates/cloud-list.html',
               controller: 'ListFilesWebCtrl'
               }
               }
               })
        
        .state('tab.saveFromWeb', {
               url: '/options/saveFromWeb',
               views: {
               'tab-options': {
               templateUrl: 'templates/cloud-local-save.html',
               controller: 'LocalSaveWebCtrl'
               }
               }
               })
        
        
        //dropbox starts here
        .state('tab.listDropbox', {
               url: '/options/listDropbox',
               views: {
               'tab-options': {
               templateUrl: 'templates/dropbox-list.html',
               controller: 'ListDropboxFilesCtrl'
               }
               }
               })
        
        .state('tab.localSave', {
               url: '/options/listDropbox/save',
               views: {
               'tab-options': {
               templateUrl: 'templates/dropbox-localSave.html',
               controller: 'DropboxLocalSaveCtrl'
               }
               }
               })
        
        .state('tab.saveToDropbox', {
               url: '/options/saveToDropbox',
               views: {
               'tab-options': {
               templateUrl: 'templates/dropbox-save.html',
               controller: 'SaveToDropboxCtrl'
               }
               }
               });
        
        // if none of the above states are matched, use this as the fallback
        //if(window.localStorage['didTutorial'] == "true") {
            $urlRouterProvider.otherwise('/tab/home');
        
        /*} else {
            $urlRouterProvider.otherwise('/intro');
        }*/
        
        });
