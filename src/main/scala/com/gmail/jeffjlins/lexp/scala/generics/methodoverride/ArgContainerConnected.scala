package com.gmail.jeffjlins.lexp.scala.generics.methodoverride

/**
 * Created by jlins on 2/5/16.
 */
//this doesn't work because A is not defined anywhere
//class ArgContainerConnected extends ArgContainerConnectedSuper[A] {

//can't do this
//class ArgContainerConnected[A] extends ArgContainerConnectedSuper[+A] {

//can't do this
//class ArgContainerConnected[+A] extends ArgContainerConnectedSuper[+A] {

//but can do this
//class ArgContainerConnected[+A] extends ArgContainerConnectedSuper[A] {
class ArgContainerConnected[A] extends ArgContainerConnectedSuper[A] {

}
