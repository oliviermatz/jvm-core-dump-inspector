/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2016 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 *
 * Contributor(s):
 *
 * Portions Copyrighted 2016 Sun Microsystems, Inc.
 */
package org.netbeans.lib.profiler.heap;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Tomas Hurka
 */
public class HprofGCRoots {

    final HprofHeap heap;
    private ThreadObjectHprofGCRoot lastThreadObjGC;
    final private Object lastThreadObjGCLock = new Object();
    private Map gcRoots;
    final private Object gcRootLock = new Object();
    private List gcRootsList;

    HprofGCRoots(HprofHeap h) {
        heap = h;
    }
    
    public Collection getGCRoots() {
        synchronized (gcRootLock) {
            if (gcRoots == null) {
                gcRoots = computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_UNKNOWN));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_JNI_GLOBAL)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_JNI_LOCAL)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_JAVA_FRAME)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_NATIVE_STACK)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_STICKY_CLASS)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_THREAD_BLOCK)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_MONITOR_USED)));
                gcRoots.putAll(computeGCRootsFor(heap.getHeapTagBound(HprofHeap.ROOT_THREAD_OBJECT)));

                gcRootsList = new ArrayList(gcRoots.values());
                Collections.sort(gcRootsList, new Comparator() {
                    public int compare(Object o1, Object o2) {
                        HprofGCRoot r1 = (HprofGCRoot) o1;
                        HprofGCRoot r2 = (HprofGCRoot) o2;
                        int kind = r1.getKind().compareTo(r2.getKind());

                        if (kind != 0) {
                            return kind;
                        }
                        return Long.compare(r1.getInstanceId(), r2.getInstanceId());
                    }
                });
            }

            return gcRootsList;
        }
    }
    
    public GCRoot getGCRoot(Long instanceId) {
        synchronized (gcRootLock) {
            if (gcRoots == null) {
                heap.getGCRoots();
            }

            return (GCRoot) gcRoots.get(instanceId);
        }
    }
    
    public ThreadObjectGCRoot getThreadGCRoot(int threadSerialNumber) {
        synchronized (lastThreadObjGCLock) { 
            if (lastThreadObjGC != null && threadSerialNumber == lastThreadObjGC.getThreadSerialNumber()) {
                return lastThreadObjGC;
            }
            
            Iterator gcRootsIt = heap.getGCRoots().iterator();

            while(gcRootsIt.hasNext()) {
                Object gcRoot = gcRootsIt.next();

                if (gcRoot instanceof ThreadObjectHprofGCRoot) {
                    ThreadObjectHprofGCRoot threadObjGC = (ThreadObjectHprofGCRoot) gcRoot;
                    if (threadSerialNumber == threadObjGC.getThreadSerialNumber()) {
                        lastThreadObjGC = threadObjGC;
                        return threadObjGC;
                    }
                }
            }
            return null;
        }
    }
    

    private Map computeGCRootsFor(TagBounds tagBounds) {
        Map roots = new HashMap();

        if (tagBounds != null) {
            int rootTag = tagBounds.tag;
            long[] offset = new long[] { tagBounds.startOffset };

            while (offset[0] < tagBounds.endOffset) {
                long start = offset[0];

                if (heap.readDumpTag(offset) == rootTag) {
                    HprofGCRoot root;
                    if (rootTag == HprofHeap.ROOT_THREAD_OBJECT) {
                        root = new ThreadObjectHprofGCRoot(this, start);                        
                    } else if (rootTag == HprofHeap.ROOT_JAVA_FRAME) {
                        root = new JavaFrameHprofGCRoot(this, start);
                    } else {
                        root = new HprofGCRoot(this, start);
                    }
                    roots.put(Long.valueOf(root.getInstanceId()), root);
                }
            }
        }
        return roots;
    }
}
