package com.gmail.jeffjlins.lexp.java.generics.methodoverride;

import java.util.List;

/**
 * Created by jlins on 1/11/16.
 */
// without the <Arg, Arg> part, the methods of this class will give weird errors
public class BaseImpl extends Base<Arg, Arg> {

    /** result variance */

//    // does not work
//    @Override
//    public ResultSuper doThings(Arg arg) {
//        return new ResultImpl();
//    }

//    // works!
//    @Override
//    public Result doThings(Arg arg) {
//        return new ResultImpl();
//    }

//    // works!
//    @Override
//    public ResultImpl doThings(Arg arg) {
//        return new ResultImpl();
//    }

    /** arg variance */

//    // does not work
//    @Override
//    public ResultImpl doThings(ArgSuper arg) {
//        return new ResultImpl();
//    }

//    // works!
//    @Override
//    public ResultImpl doThings(Arg arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doThings(ArgImpl arg) {
//        return new ResultImpl();
//    }

    /** arg variance with type parameter */

//    // does not work
//    @Override
//    public ResultImpl doThingWithTypeParam(ArgSuper arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doThingWithTypeParam(Arg arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doThingWithTypeParam(ArgImpl arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends Arg>  ResultImpl doThingWithTypeParam(T arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends ArgImpl>  ResultImpl doThingWithTypeParam(T arg) {
//        return new ResultImpl();
//    }

//    // works!
//    @Override
//    public <T>  ResultImpl doThingWithTypeParam(T arg) {
//        return new ResultImpl();
//    }

    /** arg variance with extends type parameter */

//    // does not work
//    @Override
//    public ResultImpl doThingWithExtendTypeParam(ArgSuper arg) {
//        return new ResultImpl();
//    }

//    // works!
//    @Override
//    public ResultImpl doThingWithExtendTypeParam(Arg arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doThingWithExtendTypeParam(ArgImpl arg) {
//        return new ResultImpl();
//    }

//    // works
//    @Override
//    public <T extends Arg>  ResultImpl doThingWithExtendTypeParam(T arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends ArgImpl>  ResultImpl doThingWithExtendTypeParam(T arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T>  ResultImpl doThingWithExtendTypeParam(T arg) {
//        return new ResultImpl();
//    }

    /** arg variance with generic list */

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithList(List<ArgSuper> arg) {
//        return new ResultImpl();
//    }

    // works!
    @Override
    public ResultImpl doSomethingWithList(List<Arg> arg) {
        return new ResultImpl();
    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithList(List<ArgImpl> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends Arg>  ResultImpl doSomethingWithList(List<T> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithList(List<? extends Arg> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends ArgImpl>  ResultImpl doSomethingWithList(List<T> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T>  ResultImpl doSomethingWithList(List<T> arg) {
//        return new ResultImpl();
//    }

    /** arg variance with generic wildcard extends list */

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithWildcardExtendList(List<ArgSuper> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithWildcardExtendList(List<Arg> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithWildcardExtendList(List<ArgImpl> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends Arg>  ResultImpl doSomethingWithWildcardExtendList(List<T> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithWildcardExtendList(List<Arg> arg) {
//        return new ResultImpl();
//    }

    // works!
    @Override
    public ResultImpl doSomethingWithWildcardExtendList(List<? extends Arg> arg) {
        return new ResultImpl();
    }

//    // does not work
//    @Override
//    public <T extends ArgImpl>  ResultImpl doSomethingWithWildcardExtendList(List<T> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithWildcardExtendList(List<? extends ArgImpl> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T>  ResultImpl doSomethingWithWildcardExtendList(List<T> arg) {
//        return new ResultImpl();
//    }

    /** arg variance with generic type param extends list */

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithTypeParamExtendList(List<ArgSuper> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithTypeParamExtendList(List<Arg> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithTypeParamExtendList(List<ArgImpl> arg) {
//        return new ResultImpl();
//    }

    // works!
    @Override
    public <T extends Arg>  ResultImpl doSomethingWithTypeParamExtendList(List<T> arg) {
        return new ResultImpl();
    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithTypeParamExtendList(List<? extends Arg> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T extends ArgImpl>  ResultImpl doSomethingWithTypeParamExtendList(List<T> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public ResultImpl doSomethingWithTypeParamExtendList(List<? extends ArgImpl> arg) {
//        return new ResultImpl();
//    }

//    // does not work
//    @Override
//    public <T>  ResultImpl doSomethingWithTypeParamExtendList(List<T> arg) {
//        return new ResultImpl();
//    }


}
