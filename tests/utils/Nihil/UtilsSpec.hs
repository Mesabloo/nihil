{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Nihil.UtilsSpec where

import Test.Hspec
import Nihil.Utils.Impossible
import Nihil.Utils.Debug
import Prelude hiding (error, log)

spec :: Spec
spec = do
    describe "`impossible`"            impossibleTest
    describe "unconditional debugging" debugNoCondTest
    describe "conditional debugging"   debugCondTest

impossibleTest :: Spec
impossibleTest = do
    it "should fail on call" do
        impossible "Test" `shouldThrow` anyErrorCall

debugNoCondTest :: Spec
#ifndef DEBUGGING
debugNoCondTest = do
    describe "`info`" do
        it "should not fail on `undefined` when debugging is off" do
            info  @() undefined (pure ()) `shouldReturn` ()
    describe "`warn`" do
        it "should not fail on `undefined` when debugging is off" do
            warn  @() undefined (pure ()) `shouldReturn` ()
    describe "`log`" do
        it "should not fail on `undefined` when debugging is off" do
            log   @() undefined (pure ()) `shouldReturn` ()
    describe "`error`" do
        it "should not fail on `undefined` when debugging is off" do
            error @() undefined (pure ()) `shouldReturn` ()
#else
debugNoCondTest = do
    describe "`info`" do
        it "should fail on `undefined` when debugging is on" do
            info  @() undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`warn`" do
        it "should fail on `undefined` when debugging is on" do
            warn  @() undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`log`" do
        it "should fail on `undefined` when debugging is on" do
            log   @() undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`error`" do
        it "should fail on `undefined` when debugging is on" do
            error @() undefined (pure ()) `shouldThrow` anyErrorCall
#endif

debugCondTest :: Spec
#ifndef DEBUGGING
debugCondTest = do
    describe "`infoWhen True`" do
        it "should not fail on `undefined` when debugging is off" do
            infoWhen  @() True undefined (pure ()) `shouldReturn` ()
    describe "`warnWhen True`" do
        it "should not fail on `undefined` when debugging is off" do
            warnWhen  @() True undefined (pure ()) `shouldReturn` ()
    describe "`logWhen True`" do
        it "should not fail on `undefined` when debugging is off" do
            logWhen   @() True undefined (pure ()) `shouldReturn` ()
    describe "`errorWhen True`" do
        it "should not fail on `undefined` when debugging is off" do
            errorWhen @() True undefined (pure ()) `shouldReturn` ()
#else
debugCondTest = do
    describe "`infoWhen True`" do
        it "should fail on `undefined` when debugging is on" do
            infoWhen  @() True undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`warnWhen True`" do
        it "should fail on `undefined` when debugging is on" do
            warnWhen  @() True undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`logWhen True`" do
        it "should fail on `undefined` when debugging is on" do
            logWhen   @() True undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`errorWhen True`" do
        it "should fail on `undefined` when debugging is on" do
            errorWhen @() True undefined (pure ()) `shouldThrow` anyErrorCall
    describe "`infoWhen False`" do
        it "should not fail on `undefined` when debugging is off" do
            infoWhen  @() False undefined (pure ()) `shouldReturn` ()
    describe "`warnWhen False`" do
        it "should not fail on `undefined` when debugging is off" do
            warnWhen  @() False undefined (pure ()) `shouldReturn` ()
    describe "`logWhen False`" do
        it "should not fail on `undefined` when debugging is off" do
            logWhen   @() False undefined (pure ()) `shouldReturn` ()
    describe "`errorWhen False`" do
        it "should not fail on `undefined` when debugging is off" do
            errorWhen @() False undefined (pure ()) `shouldReturn` ()
#endif