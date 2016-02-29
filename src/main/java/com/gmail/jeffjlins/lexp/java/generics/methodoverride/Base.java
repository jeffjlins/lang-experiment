package com.gmail.jeffjlins.lexp.java.generics.methodoverride;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by jlins on 1/11/16.
 */

// does not work - says S is not within its type bound
//public class Base<S extends ArgSuper, Arg> extends BaseSuper<S, Arg> {
// does not work - says S is not within its type bound
//public class Base<S extends ArgImpl, Arg> extends BaseSuper<S, Arg> {
// this seems like it works but the errors actualy show up in the overriden methods of the implementation class
//public class Base<S extends ArgImpl, Arg> extends BaseSuper<S, ArgImpl> {
// this seems like it works but the errors actualy show up in the overriden methods of the implementation class
//public class Base<S extends Arg, Arg> extends BaseSuper<S, Arg> {
public class Base<S extends Arg, Arg> extends BaseSuper<Arg, Arg> {


    public Result doThings(Arg arg) {
        return new Result();
    }

    public List<?> doSomethingElseWildcardList(Arg arg) {
        return new ArrayList<>();
    }

    public List<? extends Result> doSomethingElseWildcardExtendList(Arg arg) {
        return new ArrayList<>();
    }

    public  <T> List<T> doSomethingElseTypeParamList(Arg arg) {
        return new ArrayList<>();
    }

    public  <T extends Result> List<T> doSomethingElseTypeParamExtendList(Arg arg) {
        return new ArrayList<>();
    }

    public <T> Result doThingWithTypeParam(T arg) {
        return new Result();
    }

    public <T extends Arg> Result doThingWithExtendTypeParam(T arg) {
        return new Result();
    }

    public Result doSomethingWithList(List<Arg> arg) {
        return new Result();
    }

    public Result doSomethingWithWildcardExtendList(List<? extends Arg> arg) {
        return new Result();
    }

    public <T extends Arg> Result doSomethingWithTypeParamExtendList(List<T> arg) {
        return new Result();
    }

}
